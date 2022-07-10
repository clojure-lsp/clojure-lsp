(ns integration.client
  (:require
   [clojure.core.async :as async]
   [integration.helper :as h]
   [integration.lsp-json-rpc :as lsp-json-rpc])
  (:import
   [java.time LocalDateTime]
   [java.time.format DateTimeFormatter]))

(def ^:private ESC \u001b)

(def ^:private colors
  {:black     (str ESC "[30m")
   :red-bg    (str ESC "[41m")
   :red       (str ESC "[31m")
   :green     (str ESC "[32m")
   :yellow    (str ESC "[33m")
   :blue      (str ESC "[34m")
   :magenta   (str ESC "[35m")
   :cyan      (str ESC "[36m")
   :white     (str ESC "[37m")
   :underline (str ESC "[4m")
   :reset     (str ESC "[m")})

(defn ^:private colored [color string]
  (str (get colors color) string (:reset colors)))

(def ^:private ld-formatter DateTimeFormatter/ISO_LOCAL_DATE_TIME)
(defn ^:private local-datetime-str [] (.format ld-formatter (LocalDateTime/now)))

(defprotocol IClient
  (start [this])
  (shutdown [this])
  (exit [this])
  (send-request [this method body])
  (send-notification [this method body])
  (receive-response [this resp])
  (receive-request [this req])
  (receive-notification [this notif]))

(defprotocol IMockClient
  (mock-response [this method body]))

(defn ^:private log
  ([{:keys [client-id]} color msg params]
   (println (local-datetime-str)
            (colored color (str "Client " client-id " " msg))
            (colored :yellow params))
   (flush)))

(defn ^:private listen!
  "Read JSON-RPC messages (Clojure hashmaps) off the message channel, parse them
  as requests, responses or notifcations, and send them to the client. Returns a
  channel which will close when the messages channel closes."
  [messages client]
  (async/go-loop []
    (if-let [{:keys [id method] :as json} (async/<! messages)]
      (do
        (try
          (cond
            (and id method) (receive-request client json)
            id              (receive-response client json)
            :else           (receive-notification client json))
          (catch Throwable e
            (log client :red "listener closed:" "exception receiving")
            (println e)
            (throw e)))
        (recur))
      (log client :white "listener closed:" "server closed"))))

(defrecord TestClient [client-id
                       sender receiver
                       request-id sent-requests
                       received-requests received-notifications
                       mock-responses]
  IClient
  (start [this]
    (swap! receiver listen! this))
  (shutdown [_this] ;; simulate client closing
    (async/close! sender))
  (exit [_this] ;; wait for shutdown of server to propagate to receiver
    (async/<!! @receiver))
  (send-request [this method body]
    (let [req (lsp-json-rpc/json-rpc-message (swap! request-id inc) method body)
          p (promise)]
      (log this :cyan "sending request:" req)
      ;; Important: record request before sending it, so it is sure to be
      ;; available during receive-response.
      (swap! sent-requests assoc (:id req) p)
      (async/>!! sender req)
      p))
  (send-notification [this method body]
    (let [notif (lsp-json-rpc/json-rpc-message method body)]
      (log this :blue "sending notification:" notif)
      (async/>!! sender notif)))
  (receive-response [this {:keys [id] :as resp}]
    (if-let [request (get @sent-requests id)]
      (do (log this :green "received reponse:" resp)
          (swap! sent-requests dissoc id)
          (deliver request (if (:error resp)
                             resp
                             (:result resp))))
      ;; TODO: if we don't have a request, this will return nil, which will get
      ;; derefed, which will throw an error. Better to use promesa, and return a
      ;; rejected promise? Promesa has the benefit of using CompletableFuture,
      ;; making it more like what a lsp4j server expects.
      (log this :red "received response for unmatched request:" resp)))
  (receive-request [this {:keys [id method] :as req}]
    (log this :magenta "received request:" req)
    (swap! received-requests conj req)
    (when-let [mock-resp (get @mock-responses (keyword method))]
      (let [resp {:id id
                  :result mock-resp}]
        (log this :magenta "sending mock response:" resp)
        (async/>!! sender resp))))
  (receive-notification [this notif]
    (log this :blue "received notification:" notif)
    (swap! received-notifications conj notif))
  IMockClient
  (mock-response [_this method body]
    (swap! mock-responses assoc method body)))

(defonce client-id (atom 0))

(defn stdio-client [server-in server-out]
  (map->TestClient
    {:client-id (swap! client-id inc)
     :sender (lsp-json-rpc/buffered-writer->sender-chan server-in)
     :receiver (atom (lsp-json-rpc/buffered-reader->receiver-chan server-out))
     :request-id (atom 0)
     :sent-requests (atom {})
     :received-requests (atom [])
     :received-notifications (atom [])
     :mock-responses (atom {})}))

(defn ^:private keyname [key] (str (namespace key) "/" (name key)))

(defn ^:private await-first-and-remove! [client pred coll-type]
  (let [coll* (coll-type client)]
    (loop [tries 0]
      (if (< tries 20)
        (if-let [elem (first (filter pred @coll*))]
          (do
            (swap! coll* #(->> % (remove #{elem}) vec))
            elem)
          (do
            (Thread/sleep 500)
            (recur (inc tries))))
        (do
          (log client :red "timeout waiting:" coll-type)
          (throw (ex-info "timeout waiting for client to receive req/notif" {:coll-type coll-type})))))))

(defn await-server-diagnostics [client path]
  (let [file (h/source-path->file path)
        uri (h/file->uri file)
        method-str (keyname :textDocument/publishDiagnostics)
        notification (await-first-and-remove! client
                                              #(and (= method-str (:method %))
                                                    (= uri (-> % :params :uri)))
                                              :received-notifications)]
    (-> notification :params :diagnostics)))

(defn await-server-notification [client method]
  (let [method-str (keyname method)
        notification (await-first-and-remove! client
                                              #(= method-str (:method %))
                                              :received-notifications)]
    (:params notification)))

(defn await-server-request [client method]
  (let [method-str (keyname method)
        msg (await-first-and-remove! client
                                     #(= method-str (:method %))
                                     :received-requests)]
    (:params msg)))

(defn request-and-await-server-response! [client method body]
  (let [resp (deref (send-request client method body)
                    30000
                    ::timeout)]
    (if (= ::timeout resp)
      (do
        (log client :red "timeout waiting for server response to client request:" method)
        (throw (ex-info "timeout waiting for server response to client request" {:method method :body body})))
      resp)))

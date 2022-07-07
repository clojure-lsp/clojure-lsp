(ns integration.client
  (:require
   [cheshire.core :as json]
   [clojure.java.io :as io]
   [clojure.string :as string]
   [integration.helper :as h])
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

(defn ^:private content-length [json]
  (+ 1 (.length json)))

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
            (colored :yellow params))))

(def ^:private wire-lock (Object.))

(defn wire-send [server-in params]
  (let [content (json/generate-string params)]
    (binding [*out* server-in]
      (locking wire-lock
        (println (str "Content-Length: " (content-length content)))
        (println "")
        (println content)
        (flush)))))

(defn ^:private lsp-rpc [{:keys [request-id]} method params]
  {:jsonrpc "2.0"
   :method method
   :params params
   :id (swap! request-id inc)})

(defn read-n-chars [^java.io.Reader input content-length]
  (let [cs (char-array content-length)]
    (loop [total-read 0]
      (when (< total-read content-length)
        (let [new-read (.read input cs total-read (- content-length total-read))]
          (when (< new-read 0)
            (throw (ex-info "no content" {})))
          (recur (+ total-read new-read)))))
    (String. cs)))

(defn read-content-length [input]
  (binding [*in* input]
    (when-let [line (read-line)] ;; returns nil when input is closed, i.e. server output has closed
      (let [[h v] (string/split line #":")]
        (when-not (= h "Content-Length")
          (throw (ex-info "unexpected header" {:line line})))
        (parse-long (string/trim v))))))

(defn ^:private listen! [server-out client]
  (try
    (loop []
      ;; Block, waiting for next Content-Length line, then parse the number of
      ;; characters specified as JSON-RPC. If the server output stream is
      ;; closed, also close the client by exiting this loop.
      (if-let [content-length (read-content-length server-out)]
        ;; NOTE: this doesn't attempt to handle Content-Type header
        (let [content-length (+ 2 content-length) ;; include \r\n before message
              content (read-n-chars server-out content-length)
              {:keys [id method] :as json} (cheshire.core/parse-string content true)]
          (cond
            (and id method) (receive-request client json)
            id              (receive-response client json)
            :else           (receive-notification client json))
          (recur))
        (do
          (log client :white "listener closed:" "server closed")
          (flush))))
    (catch Throwable e
      (log client :red "listener closed:" "exception")
      (println e)
      (throw e))))

(defrecord Client [client-id
                   server-in server-out
                   listener
                   request-id sent-requests
                   received-requests received-notifications
                   mock-responses]
  IClient
  (start [this]
    (reset! listener (future (listen! server-out this))))
  (shutdown [_this] ;; simulate client closing
    (.close server-in))
  (exit [_this] ;; wait for shutdown of server to propagate to listener
    @@listener)
  (send-request [this method body]
    (let [req (lsp-rpc this method body)
          p (promise)]
      (log this :cyan "sending request:" req)
      ;; Important: record request before sending it, so it is sure to be
      ;; available during receive-response.
      (swap! sent-requests assoc (:id req) p)
      (wire-send server-in req)
      p))
  (send-notification [this method body]
    (let [notif (lsp-rpc this method body)]
      (log this :blue "sending notification:" notif)
      (wire-send server-in notif)))
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
        (wire-send server-in resp))))
  (receive-notification [this notif]
    (log this :blue "received notification:" notif)
    (swap! received-notifications conj notif))
  IMockClient
  (mock-response [_this method body]
    (swap! mock-responses assoc method body)))

(defonce client-id (atom 0))

(defn client [server-in server-out]
  (map->Client
    {:client-id (swap! client-id inc)
     :server-in server-in
     :server-out server-out
     :request-id (atom 0)
     :sent-requests (atom {})
     :received-requests (atom [])
     :received-notifications (atom [])
     :mock-responses (atom {})
     :listener (atom nil)}))

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

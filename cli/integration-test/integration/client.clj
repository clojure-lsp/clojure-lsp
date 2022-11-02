(ns integration.client
  (:require
   [clojure.core.async :as async]
   [clojure.string :as string]
   [integration.helper :as h]
   [lsp4clj.coercer :as coercer]
   [lsp4clj.io-chan :as lsp.io-chan]
   [lsp4clj.lsp.requests :as lsp.requests]
   [lsp4clj.lsp.responses :as lsp.responses]
   [lsp4clj.protocols.endpoint :as protocols.endpoint])
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

(defprotocol IMockClient
  (mock-response [this method body]))

(defn ^:private format-log
  [{:keys [client-id]} color msg params]
  (string/join " "
               [(local-datetime-str)
                (colored color (str "Client " client-id " " msg))
                (colored :yellow params)]))

(defn ^:private receive-message
  [client context message]
  (let [message-type (coercer/input-message-type message)]
    (try
      (let [response
            (case message-type
              (:parse-error :invalid-request)
              (protocols.endpoint/log client :red "Error reading message" message-type)
              :request
              (protocols.endpoint/receive-request client context message)
              (:response.result :response.error)
              (protocols.endpoint/receive-response client message)
              :notification
              (protocols.endpoint/receive-notification client context message))]
        ;; Ensure client only responds to requests
        (when (identical? :request message-type)
          response))
      (catch Throwable e
        (protocols.endpoint/log client :red "Error receiving:" e)
        (throw e)))))

(defrecord Client [client-id
                   input output
                   log-ch
                   join
                   request-id sent-requests
                   received-requests received-notifications
                   mock-responses]
  protocols.endpoint/IEndpoint
  (start [this context]
    (protocols.endpoint/log this :white "lifecycle:" "starting")
    (let [pipeline (async/pipeline-blocking
                     1 ;; no parallelism preserves server message order
                     output
                     ;; TODO: return error until initialize request is received? https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#initialize
                     ;; `keep` means we do not reply to responses and notifications
                     (keep #(receive-message this context %))
                     input)]
      (async/go
        ;; wait for pipeline to close, indicating input closed
        (async/<! pipeline)
        (deliver join :done)))
    ;; invokers can deref the return of `start` to stay alive until server is
    ;; shut down
    join)
  (shutdown [this]
    (protocols.endpoint/log this :white "lifecycle:" "shutting down")
    ;; closing input will drain pipeline, then close output, then close
    ;; pipeline
    (async/close! input)
    (if (= :done (deref join 10e3 :timeout))
      (protocols.endpoint/log this :white "lifecycle:" "shutdown")
      (protocols.endpoint/log this :red "lifecycle:" "shutdown timed out"))
    (async/close! log-ch))
  (log [this msg params]
    (protocols.endpoint/log this :white msg params))
  (log [this color msg params]
    (async/put! log-ch (format-log this color msg params)))
  (send-request [this method body]
    (let [req (lsp.requests/request (swap! request-id inc) method body)
          p (promise)
          start-ns (System/nanoTime)]
      (protocols.endpoint/log this :cyan "sending request:" req)
      ;; Important: record request before sending it, so it is sure to be
      ;; available during receive-response.
      (swap! sent-requests assoc (:id req) {:request p
                                            :start-ns start-ns})
      (async/>!! output req)
      p))
  (send-notification [this method body]
    (let [notif (lsp.requests/notification method body)]
      (protocols.endpoint/log this :blue "sending notification:" notif)
      (async/>!! output notif)))
  (receive-response [this {:keys [id] :as resp}]
    (if-let [{:keys [request start-ns]} (get @sent-requests id)]
      (let [ms (float (/ (- (System/nanoTime) start-ns) 1000000))]
        (protocols.endpoint/log this :green (format "received response (%.0fms):" ms) resp)
        (swap! sent-requests dissoc id)
        (deliver request (if (:error resp)
                           resp
                           (:result resp))))
      (protocols.endpoint/log this :red "received response for unmatched request:" resp)))
  (receive-request [this _ {:keys [id method] :as req}]
    (protocols.endpoint/log this :magenta "received request:" req)
    (swap! received-requests conj req)
    (when-let [mock-resp (get @mock-responses (keyword method))]
      (let [resp (lsp.responses/response id mock-resp)]
        (protocols.endpoint/log this :magenta "sending mock response:" resp)
        resp)))
  (receive-notification [this _ notif]
    (protocols.endpoint/log this :blue "received notification:" notif)
    (swap! received-notifications conj notif))
  IMockClient
  (mock-response [_this method body]
    (swap! mock-responses assoc method body)))

(defonce client-id (atom 0))

(defn client [server-in server-out]
  (map->Client
    {:client-id (swap! client-id inc)
     :input (lsp.io-chan/input-stream->input-chan server-out {:keyword-function keyword})
     :output (lsp.io-chan/output-stream->output-chan server-in)
     :log-ch (async/chan (async/sliding-buffer 20))
     :join (promise)
     :request-id (atom 0)
     :sent-requests (atom {})
     :received-requests (atom [])
     :received-notifications (atom [])
     :mock-responses (atom {})}))

(def start protocols.endpoint/start)
(def shutdown protocols.endpoint/shutdown)
(def send-notification protocols.endpoint/send-notification)

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
          (protocols.endpoint/log client :red "timeout waiting:" coll-type)
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
  (let [timeout-ms 180000
        resp (deref (protocols.endpoint/send-request client method body)
                    timeout-ms
                    ::timeout)]
    (if (= ::timeout resp)
      (do
        (protocols.endpoint/log client :red "timeout waiting for server response to client request:"
                                {:method method :timeout-ms timeout-ms})
        (throw (ex-info "timeout waiting for server response to client request" {:method method :body body})))
      resp)))

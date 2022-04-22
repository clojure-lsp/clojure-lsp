(ns integration.lsp
  (:require
   [babashka.process :as p]
   [cheshire.core :as json]
   [clojure.java.io :as io]
   [clojure.test :refer [use-fixtures]]
   [integration.helper :as h])
  (:import
   [java.time.format DateTimeFormatter]
   [java.time LocalDateTime]))

(def ^:dynamic *clojure-lsp-process* nil)
(def ^:dynamic *clojure-lsp-listener* nil)
(def ^:dynamic *server-in* nil)
(def ^:dynamic *server-out* nil)

(defonce server-responses (atom {}))
(defonce server-requests (atom []))
(defonce server-notifications (atom []))
(defonce client-id (atom 0))
(defonce client-request-id (atom 0))

(defn inc-request-id []
  (swap! client-request-id inc))

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

(defn ^:private keyname [key] (str (namespace key) "/" (name key)))

(def ^:private ld-formatter DateTimeFormatter/ISO_LOCAL_DATE_TIME)
(defn ld-str [] (.format ld-formatter (LocalDateTime/now)))

(defn client-log
  ([color msg params]
   (client-log @client-id color msg params))
  ([client-id color msg params]
   (println (ld-str) (colored color (str "Client " client-id " " msg)) (colored :yellow params))))

(defn ^:private listen-output! []
  (let [client-id (swap! client-id inc)]
    (future
      (try
        (binding [*in* *server-out*]
          (loop []
            ;; Block, waiting for next Content-Length line, then discard it. If
            ;; the server output stream is closed, also close the client by
            ;; exiting this loop.
            (if-let [_content-length (read-line)]
              (let [{:keys [id method] :as json} (cheshire.core/parse-stream *in* true)]
                (cond
                  (and id method)
                  (do
                    (client-log client-id :magenta "received request:" json)
                    (swap! server-requests conj json))

                  id
                  (do
                    (client-log client-id :green "received reponse:" json)
                    (swap! server-responses assoc id json))

                  :else
                  (do
                    (client-log client-id :blue "received notification:" json)
                    (swap! server-notifications conj json)))
                (recur))
              (do
                (client-log client-id :red "closed:" "server closed")
                (flush)))))
        (catch Throwable e
          (client-log client-id :red "closed:" "exception")
          (println e)
          (throw e))))))

(defn start-process! []
  (let [clojure-lsp-binary (first *command-line-args*)]
    (alter-var-root #'*clojure-lsp-process* (constantly (p/process [(.getCanonicalPath (io/file clojure-lsp-binary))] {:dir "integration-test/sample-test/"})))
    (alter-var-root #'*server-in* (constantly (io/writer (:in *clojure-lsp-process*))))
    (alter-var-root #'*server-out* (constantly (io/reader (:out *clojure-lsp-process*))))
    (alter-var-root #'*clojure-lsp-listener* (constantly (listen-output!)))))

(defn cli! [& args]
  (let [clojure-lsp-binary (first *command-line-args*)]
    (alter-var-root #'*clojure-lsp-process* (constantly (p/process (into [(.getCanonicalPath (io/file clojure-lsp-binary))] args) {:dir "integration-test/sample-test/"})))
    (io/reader (:out *clojure-lsp-process*))))

(defn clean! []
  (reset! server-requests [])
  (reset! server-responses {})
  (reset! server-notifications [])
  (reset! client-request-id 0)
  (flush)
  (when *clojure-lsp-process*
    (.close *server-in*) ;; simulate client closing
    (deref *clojure-lsp-process*) ;; wait for close of client to shutdown server
    (alter-var-root #'*clojure-lsp-process* (constantly nil)))
  (when *clojure-lsp-listener*
    (deref *clojure-lsp-listener*) ;; wait for shutdown of server to propagate to listener
    (alter-var-root #'*clojure-lsp-listener* (constantly nil))))

(defn clean-after-test []
  (use-fixtures :each (fn [f] (clean!) (f)))
  (use-fixtures :once (fn [f] (f) (clean!))))

(defn client-send [params]
  (let [content (json/generate-string params)]
    (binding [*out* *server-in*]
      (println (str "Content-Length: " (content-length content)))
      (println "")
      (println content)
      (flush))))

(defn notify! [params]
  (client-log :blue "sending notification:" params)
  (client-send params))

(defn request! [params]
  (client-log :cyan "sending request:" params)
  (client-send params)
  (loop []
    (if-let [response (get @server-responses (:id params))]
      (do
        (swap! server-responses dissoc (:id params))
        (if (:error response)
          response
          (:result response)))
      (do
        (Thread/sleep 500)
        (recur)))))

(defn await-first-and-remove! [coll* pred]
  (loop []
    (if-let [elem (first (filter pred @coll*))]
      (do
        (swap! coll* #(->> % (remove #{elem}) vec))
        elem)
      (do
        (Thread/sleep 500)
        (recur)))))

(defn await-diagnostics [path]
  (let [file (h/source-path->file path)
        uri (h/file->uri file)
        method-str (keyname :textDocument/publishDiagnostics)
        notification (await-first-and-remove! server-notifications
                                              #(and (= method-str (:method %))
                                                    (= uri (-> % :params :uri))))]
    (-> notification :params :diagnostics)))

(defn await-notification [method]
  (let [method-str (keyname method)
        notification (await-first-and-remove! server-notifications
                                              #(= method-str (:method %)))]
    (:params notification)))

(defn await-client-request [method]
  (let [method-str (keyname method)
        msg (await-first-and-remove! server-requests
                                     #(= method-str (:method %)))]
    (:params msg)))

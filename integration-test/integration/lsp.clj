(ns integration.lsp
  (:require
    [babashka.process :as p]
    [cheshire.core :as json]
    [clojure.core.async :as async]
    [clojure.java.io :as io]
    [clojure.test :refer [use-fixtures]]
    [integration.helper :as h]))

(def ^:dynamic *clojure-lsp-process* nil)
(def ^:dynamic *clojure-lsp-listener* nil)
(def ^:dynamic *stdin* nil)
(def ^:dynamic *stdout* nil)

(defonce server-responses (atom {}))
(defonce server-requests (atom {}))
(defonce server-notifications (atom []))
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

(defn ^:private listen-output! []
  (async/thread
    (try
      (loop []
        (binding [*in* *stdout*]
          (let [_content-length (read-line)
                {:keys [id method] :as json} (cheshire.core/parse-stream *in* true)]
            (cond
              (and id method)
              (do
                (println (colored :magenta "Received request:") (colored :yellow json))
                (swap! server-requests assoc id json))

              id
              (do
                (println (colored :green "Received response:") (colored :yellow json))
                (swap! server-responses assoc id json))

              :else
              (do
                (println (colored :blue "Received notification:") (colored :yellow json))
                (swap! server-notifications conj json)))))
        (recur))
      (catch Exception _))))

(defn start-process! []
  (let [clojure-lsp-binary (first *command-line-args*)]
    (alter-var-root #'*clojure-lsp-process* (constantly (p/process [clojure-lsp-binary])))
    (alter-var-root #'*stdin* (constantly (io/writer (:in *clojure-lsp-process*))))
    (alter-var-root #'*stdout* (constantly (io/reader (:out *clojure-lsp-process*))))
    (alter-var-root #'*clojure-lsp-listener* (constantly (listen-output!)))))

(defn cli! [& args]
  (let [clojure-lsp-binary (first *command-line-args*)]
    (alter-var-root #'*clojure-lsp-process* (constantly (p/process (into [clojure-lsp-binary] args))))
    (io/reader (:out *clojure-lsp-process*))))

(defn clean! []
  (reset! server-requests {})
  (reset! server-responses {})
  (reset! server-notifications [])
  (reset! client-request-id 0)
  (when *clojure-lsp-listener*
    (async/close! *clojure-lsp-listener*))
  (when *clojure-lsp-process*
    (p/destroy *clojure-lsp-process*)))

(defn clean-after-test []
  (use-fixtures :each (fn [f] (clean!) (f)))
  (use-fixtures :once (fn [f] (f) (clean!))))

(defn notify! [params]
  (println (colored :blue "Sending notification:") (colored :yellow params))
  (binding [*out* *stdin*]
    (println (str "Content-Length: " (content-length params)))
    (println "")
    (println params)))

(defn request! [params]
  (println (colored :cyan "Sending request:") (colored :yellow params))
  (binding [*out* *stdin*]
    (println (str "Content-Length: " (content-length params)))
    (println "")
    (println params))
  (loop [response (get @server-responses @client-request-id)]
    (if response
      (do
        (swap! server-responses dissoc @client-request-id)
        (:result response))
      (do
        (Thread/sleep 500)
        (recur (get @server-responses @client-request-id))))))

(defn await-notification [method]
  (loop []
    (let [method-str (keyname method)
          notification (first (filter #(= method-str (:method %)) @server-notifications))]
      (if notification
        (do
          (swap! server-notifications
                 (fn [n]
                   (->> n
                        (remove #(= method-str (:method %)))
                        vec)))
          (:params notification))
        (do
          (Thread/sleep 500)
          (recur))))))

(defn await-diagnostics [path]
  (let [file (h/source-path->file path)
        uri (h/file->uri file)
        method-str (keyname :textDocument/publishDiagnostics)]
  (loop []
    (let [notification (first (filter #(and (= method-str (:method %))
                                            (= uri (-> % :params :uri)))
                                      @server-notifications))]
      (if notification
        (do
          (swap! server-notifications
                 (fn [n]
                   (->> n
                        (remove #(= method-str (:method %)))
                        vec)))
          (-> notification :params :diagnostics))
        (do
          (Thread/sleep 500)
          (recur)))))))

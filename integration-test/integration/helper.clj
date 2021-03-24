(ns integration.helper
  (:require
    [babashka.process :refer [process]]
    [cheshire.core :as json]
    [clojure.core.async :as async]
    [clojure.java.io :as io]
    [clojure.test :refer [is]]))

(def ^:dynamic *clojure-lsp-process*)
(def ^:dynamic *stdin*)
(def ^:dynamic *stdout*)

(def responses (atom {}))

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

(defn ^:private listen-responses! []
  (async/thread
    (binding [*in* *stdout*]
      (loop [_content-length (read-line)
             json (cheshire.core/parse-stream *in*)]
        (println (colored :green "Received response:") json)
        (swap! responses assoc (get json "id") json)
        (recur (read-line)
               (cheshire.core/parse-stream *in*))))))

(defn start-process! []
  (let [clojure-lsp-binary (first *command-line-args*)]
    (alter-var-root #'integration.helper/*clojure-lsp-process* (constantly (process [clojure-lsp-binary])))
    (alter-var-root #'integration.helper/*stdin* (constantly (io/writer (:in *clojure-lsp-process*))))
    (alter-var-root #'integration.helper/*stdout* (constantly (io/reader (:out *clojure-lsp-process*)))))
  (listen-responses!))

(defn assert-submap [expected actual]
  (is (= expected
         (some-> actual (select-keys (keys expected))))
      (str "No superset of " (pr-str actual) " found")))

(def root-project-path
  (-> (io/file *file*)
      .getParentFile
      .getParentFile
      .toPath
      (.resolve "sample-test")
      str))

(defn request! [params]
  (println (colored :cyan "Sending request:") params)
  (binding [*out* *stdin*]
    (println (str "Content-Length: " (content-length params)))
    (println "")
    (println params)))

(defn await-response! [request-id]
  (loop [response (get @responses request-id)
         tries 0]
    (if response
      (do
        (swap! responses dissoc request-id)
        response)
      (when-not (> tries 10)
        (Thread/sleep 500)
        (recur (get @responses request-id)
               (inc tries))))))

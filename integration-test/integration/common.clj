(ns integration.common
  (:require
   [babashka.process :refer [process]]
   [clojure.java.io :as io]
   [clojure.test :refer [is]]))


(def ^:dynamic *clojure-lsp-process*)
(def ^:dynamic *stdin*)
(def ^:dynamic *stdout*)

(defn start-process! []
  (let [clojure-lsp-binary (first *command-line-args*)]
    (alter-var-root #'integration.common/*clojure-lsp-process* (constantly (process [clojure-lsp-binary])))
    (alter-var-root #'integration.common/*stdin* (constantly (io/writer (:in *clojure-lsp-process*))))
    (alter-var-root #'integration.common/*stdout* (constantly (io/reader (:out *clojure-lsp-process*))))))

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

(defn content-length [json]
  (+ 1 (.length json)))

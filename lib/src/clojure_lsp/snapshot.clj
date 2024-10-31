(ns clojure-lsp.snapshot
  (:require
   [clojure-lsp.shared :as shared]
   [clojure.java.io :as io]
   [clojure.string :as str]))

(defn ^:private read-file
  ([] (read-file (io/file ".lsp" "snapshot.txt")))
  ([path]
   (shared/logging-time
     "[SNAPSHOT] Read took %s"
     (let [file (io/file path)]
       (when (.exists file)
         (with-open [reader (io/reader file)]
           (reduce (fn [acc item]
                     (let [path (-> item (str/split #":") first)]
                       (update acc path (fnil conj #{}) item)))
                   {}
                   (line-seq reader))))))))

(defonce cache (atom nil))

(defn warm-cache!
  []
  (swap! cache (fn [_] (read-file))))

(defn discard
  [relative-path]
  (get @cache relative-path #{}))

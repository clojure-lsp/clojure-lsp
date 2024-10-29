(ns clojure-lsp.snapshot
  (:require
   [clojure-lsp.shared :as shared]
   [clojure.java.io :as io]
   [clojure.string :as str])
  (:import
   (java.security MessageDigest)
   [java.util UUID]))

(defn string-to-uuid [s]
  (let [md5 (MessageDigest/getInstance "MD5")
        hash (.digest md5 (.getBytes s "UTF-8"))
        msb (->> (take 8 hash)
                 (map #(bit-and % 0xFF))
                 (reduce (fn [acc b] (bit-or (bit-shift-left acc 8) b)) 0))
        lsb (->> (drop 8 hash)
                 (map #(bit-and % 0xFF))
                 (reduce (fn [acc b] (bit-or (bit-shift-left acc 8) b)) 0))]
    (UUID. msb lsb)))

(defn ^:private string-to-md5
  [input-str]
  (let [algorithm (MessageDigest/getInstance "MD5")
        raw (.digest algorithm (.getBytes input-str "UTF-8"))]
    (format "%032x" (BigInteger. 1 raw))))

(defn ^:private read-file
  ([] (read-file (io/file ".lsp" "snapshot.txt")))
  ([path]
   (shared/logging-time
     "[SNAPSHOT] Read took %s"
     (let [file (io/file path)]
       (if (.exists file)
         (with-open [reader (io/reader file)]
           (reduce (fn [acc item]
                     (let [path (-> item (str/split #":") first)]
                       (update acc path (fnil conj #{}) item)))
                   {}
                   (line-seq reader)))
         {})))))

(defonce cache (atom {}))

(defn warm-cache!
  []
  (swap! cache (fn [_] (read-file))))

(defn discard
  [relative-path]
  (get @cache relative-path #{}))

(ns clojure-lsp.snapshot
  (:require
   [clojure-lsp.shared :as shared]
   [clojure.java.io :as io])
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
         (let [result-hash (with-open [reader (io/reader file)]
                             (into #{}
                                   (map #(keyword (string-to-md5 %))
                                        (line-seq reader))))]
           #_(spit "snapshot-uuid.edn" (pr-str result-hash))
           result-hash)
         #{})))))

(def read-file-memo (memoize read-file))

(defonce cache (atom #{}))

(defn warm-cache!
  []
  (swap! cache (fn [_] (read-file))))

(defn severity->level [severity]
  (case (int severity)
    1 :error
    2 :warning
    3 :info))

(defn diagnostic->diagnostic-message [path {:keys [message severity range code]}]
  (format "%s:%s:%s: %s: [%s] %s"
          path
          (-> range :start :line inc)
          (-> range :start :character inc)
          (name (severity->level severity))
          code
          message))

(defn discard
  [uri db diagnostics]
  (shared/logging-time
    "[SNAPSHOT] Discard took %s"
    (let [snapshot @cache
          project-path (shared/uri->filename (shared/project-root->uri nil db))
          filename (shared/uri->filename uri)
          file-output (shared/relativize-filepath filename project-path)]
      (into []
            (remove #(contains? snapshot (keyword (string-to-md5 (diagnostic->diagnostic-message file-output %))))
                    diagnostics)))))

(ns clojure-lsp.snapshot
  (:require
   [clojure-lsp.shared :as shared]
   [clojure.java.io :as io])
  (:import
   [java.io File]
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
   (let [file (io/file path)]
     (if (.exists file)
       (let [result-hash (with-open [reader (io/reader file)]
                           (into #{}
                                 (map #(string-to-uuid %)
                                      (line-seq reader))))]
         #_(spit "snapshot-uuid.edn" (pr-str result-hash))
         result-hash)
       #{}))))

(def read-file-memo (memoize read-file))

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

(defn ^:private project-root->uri [project-root db]
  (-> (or ^File project-root (io/file ""))
      .getCanonicalPath
      (shared/filename->uri db)))

(defn discard
  [uri db diagnostics]
  (shared/logging-time
    "Discarding diagnostics took %s"
    (let [snapshot (read-file-memo)
          project-path (shared/uri->filename (project-root->uri nil db))
          filename (shared/uri->filename uri)
          file-output (shared/relativize-filepath filename project-path)]
      (into []
            (remove #(contains? snapshot (string-to-uuid (diagnostic->diagnostic-message file-output %)))
                    diagnostics)))))

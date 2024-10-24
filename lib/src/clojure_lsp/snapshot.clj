(ns clojure-lsp.snapshot
  (:require
   [clojure-lsp.shared :as shared]
   [clojure.java.io :as io])
  (:import
   [java.io File]
   (java.security MessageDigest)))

(defn md5
  [input-str]
  (let [algorithm (MessageDigest/getInstance "MD5")
        raw (.digest algorithm (.getBytes input-str "UTF-8"))]
    (format "%032x" (BigInteger. 1 raw))))

(defn read-db
  ([] (read-db ".lsp/snapshot.txt"))
  ([path]
   (let [file (io/file path)]
     (if (.exists file)
       (let [result-hash (with-open [reader (io/reader file)]
                           (into #{}
                                 (map #(keyword (md5 %))
                                      (line-seq reader))))]
         #_(spit "snapshot-set.edn" (pr-str result-hash))
         result-hash)
       #{}))))

(def read-db-memo (memoize read-db))

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
  (let [snapshot (read-db-memo)
        project-path (shared/uri->filename (project-root->uri nil db))
        filename (shared/uri->filename uri)
        file-output (shared/relativize-filepath filename project-path)]
    (remove #(contains? snapshot (keyword (md5 (diagnostic->diagnostic-message file-output %))))
            diagnostics)))

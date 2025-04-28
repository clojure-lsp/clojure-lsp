(ns clojure-lsp.diff
  (:require
   [clojure-lsp.shared :as shared]
   [clojure.string :as string])
  (:import
   [difflib DiffUtils]))

(set! *warn-on-reflection* true)

(defn- lines
  "Splits S on `\n` or `\r\n`."
  [s]
  (string/split-lines s))

(defn- unlines
  "Joins SS strings coll using the system's line separator."
  [ss]
  (string/join shared/line-separator ss))

(defn unified-diff
  ([old-uri new-uri original revised project-root-uri]
   (unified-diff old-uri new-uri original revised project-root-uri 3))
  ([old-uri new-uri original revised project-root-uri context]
   (let [project-path (shared/uri->filename project-root-uri)]
     (unlines (DiffUtils/generateUnifiedDiff
                (str "a/" (-> old-uri
                              shared/uri->filename
                              (shared/relativize-filepath project-path)))
                (str "b/" (-> new-uri
                              shared/uri->filename
                              (shared/relativize-filepath project-path)))
                (lines original)
                (DiffUtils/diff (lines original) (lines revised))
                context)))))

(defn rename-diff
  [old-uri new-uri project-root-uri]
  (let [project-path (shared/uri->filename project-root-uri)]
    (->> [(str "rename from " (-> old-uri
                                  shared/uri->filename
                                  (shared/relativize-filepath project-path)))
          (str "rename to " (-> new-uri
                                shared/uri->filename
                                (shared/relativize-filepath project-path)))]
         unlines)))

(defn colorize-diff [diff-text]
  (-> diff-text
      (string/replace #"(?m)^(rename from .*)$"  (shared/colorize "$1" :yellow))
      (string/replace #"(?m)^(rename to .*)$"  (shared/colorize "$1" :yellow))
      (string/replace #"(?m)^(\-\-\-\sa.*)$"  (shared/colorize "$1" :yellow))
      (string/replace #"(?m)^(\+\+\+\sb.*)$"  (shared/colorize "$1" :yellow))
      (string/replace #"(?m)^(@@.*@@)$"       (shared/colorize "$1" :cyan))
      (string/replace #"(?m)^(\+(?!\+\+).*)$" (shared/colorize "$1" :green))
      (string/replace #"(?m)^(-(?!--).*)$"    (shared/colorize "$1" :red))))

(defn ->chunks
  "Given a diff output text,
   return a sequence of maps with the following keys:
   - :file - the file name
   - :diff - the diff content
   - :content - the content of the diff
   - :hunk-additions-start - the starting line number of the diff
   - :hunk-additions-end - the ending line number of the diff
   - :added-line-numbers - set of added line numbers"
  [diff-output]
  (let [file-fn (fn [diff]
                  (let [[_ file] (re-find #"^a/([^ ]+)" diff)]
                    {:file file
                     :diff diff}))
        files (->> (string/split (str "\n" diff-output) #"\ndiff --git ")
                   (remove string/blank?)
                   (map file-fn))
        assoc-content-fn (fn [{diff :diff :as file}]
                           (map #(assoc file :content (str "@@ " %))
                                (rest (string/split diff #"\n@@ "))))
        chunks (mapcat assoc-content-fn files)
        parse-fn (fn [{hunk-header :content :as chunk}]
                   (let [single-line-range (re-find #"@@ -\d+ \+(\d+)" hunk-header)
                         multiple-lines-range (re-find #"@@ -\d+,\d+ \+(\d+),(\d+)" hunk-header)
                         [_ added-lines-start added-lines-span] (or single-line-range
                                                                    multiple-lines-range)
                         additions-start (Integer/parseInt added-lines-start)
                         additions-span (if (nil? added-lines-span)
                                          0
                                          (Integer/parseInt added-lines-span))
                         hunk-content (rest (string/split-lines hunk-header))
                         added-lines (loop [[head & tail] hunk-content
                                            current-line additions-start
                                            result []]
                                       (if (nil? head)
                                         result
                                         (cond (string/starts-with? head "+")
                                               (recur tail (inc current-line) (conj result current-line))
                                               (string/starts-with? head " ")
                                               (recur tail (inc current-line) result)
                                               :else
                                               (recur tail current-line result))))]
                     (assoc chunk
                            :hunk-additions-start additions-start
                            :hunk-additions-end (+ additions-start additions-span)
                            :added-line-numbers (set added-lines))))
        hunks (map parse-fn chunks)]
    hunks))

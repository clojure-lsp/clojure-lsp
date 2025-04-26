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
   - :row-start - the starting line number of the diff
   - :row-end - the ending line number of the diff
   - :new-start - the starting line number of the new content
   - :new-count - the number of lines in the new content"
  [diff-output]
  (let [file-fn (fn [diff]
                  (let [[_ file] (re-find #"^a/([^ ]+)" diff)]
                    {:file file
                     :diff diff}))
        files (->> (string/split (str "\n" diff-output) #"\ndiff --git ")
                   (remove string/blank?)
                   (map file-fn))
        assoc-content-fn (fn [{diff :diff :as file}]
                           (map #(assoc file :content (str "@@ " %)) (rest (string/split diff #"\n@@ "))))
        chunks (mapcat assoc-content-fn files)
        parse-fn (fn [{content :content :as chunk}]
                   (let [single (re-find #"@@ -\d+ \+(\d+)" content)
                         multiple (re-find #"@@ -\d+,\d+ \+(\d+),(\d+)" content)
                         [_ new-start new-count] (or single multiple)
                         new-start' (try (Integer. new-start) (catch Exception _ -999999))
                         new-count' (if (nil? new-count)
                                      0
                                      (try (Integer. new-count) (catch Exception _ -888888)))]
                     (assoc chunk
                            :row-start new-start'
                            :row-end (+ new-start' new-count')
                            :new-start new-start'
                            :new-count new-count')))
        details (map parse-fn chunks)
        only-additions (remove (fn [{:keys [row-start row-end]}]
                                 (and (zero? row-start)
                                      (zero? row-end)))
                               details)]
    only-additions))

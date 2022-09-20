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

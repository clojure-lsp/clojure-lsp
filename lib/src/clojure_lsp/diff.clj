(ns clojure-lsp.diff
  (:require
   [clojure-lsp.shared :as shared]
   [clojure.string :as str])
  (:import
   [difflib DiffUtils]))

(set! *warn-on-reflection* true)

(defn- lines [s]
  (str/split s #"\n"))

(defn- unlines [ss]
  (str/join "\n" ss))

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
      (str/replace #"(?m)^(rename from .*)$"  (shared/colorize "$1" :yellow))
      (str/replace #"(?m)^(rename to .*)$"  (shared/colorize "$1" :yellow))
      (str/replace #"(?m)^(\-\-\-\sa.*)$"  (shared/colorize "$1" :yellow))
      (str/replace #"(?m)^(\+\+\+\sb.*)$"  (shared/colorize "$1" :yellow))
      (str/replace #"(?m)^(@@.*@@)$"       (shared/colorize "$1" :cyan))
      (str/replace #"(?m)^(\+(?!\+\+).*)$" (shared/colorize "$1" :green))
      (str/replace #"(?m)^(-(?!--).*)$"    (shared/colorize "$1" :red))))

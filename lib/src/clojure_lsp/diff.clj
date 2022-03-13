(ns clojure-lsp.diff
  (:require
   [clojure.string :as str]
   [clojure-lsp.shared :as shared])
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

(def ^:private ansi-colors
  {:reset "[0m"
   :red   "[31m"
   :green "[32m"
   :yellow "[33m"
   :cyan  "[36m"})

(defn colorize [s color]
  (str \u001b (ansi-colors color) s \u001b (ansi-colors :reset)))

(defn colorize-diff [diff-text]
  (-> diff-text
      (str/replace #"(?m)^(rename from .*)$"  (colorize "$1" :yellow))
      (str/replace #"(?m)^(rename to .*)$"  (colorize "$1" :yellow))
      (str/replace #"(?m)^(\-\-\-\sa.*)$"  (colorize "$1" :yellow))
      (str/replace #"(?m)^(\+\+\+\sb.*)$"  (colorize "$1" :yellow))
      (str/replace #"(?m)^(@@.*@@)$"       (colorize "$1" :cyan))
      (str/replace #"(?m)^(\+(?!\+\+).*)$" (colorize "$1" :green))
      (str/replace #"(?m)^(-(?!--).*)$"    (colorize "$1" :red))))

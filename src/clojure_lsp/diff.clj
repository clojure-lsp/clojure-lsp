(ns clojure-lsp.diff
  (:require
   [clojure-lsp.shared :as shared]
   [clojure.string :as str])
  (:import
   [difflib DiffUtils]))

(defn- lines [s]
  (str/split s #"\n"))

(defn- unlines [ss]
  (str/join "\n" ss))

(defn unified-diff
  ([uri original revised]
   (unified-diff uri original revised 3))
  ([uri original revised context]
   (unlines (DiffUtils/generateUnifiedDiff
              (->> uri shared/uri->filename (str "a"))
              (->> uri shared/uri->filename (str "b"))
              (lines original)
              (DiffUtils/diff (lines original) (lines revised))
              context))))

(def ^:private ansi-colors
  {:reset "[0m"
   :red   "[031m"
   :green "[032m"
   :yellow "[033m"
   :cyan  "[036m"})

(defn- colorize [s color]
  (str \u001b (ansi-colors color) s \u001b (ansi-colors :reset)))

(defn colorize-diff [diff-text]
  (-> diff-text
      (str/replace #"(?m)^(\-\-\-\sa.*)$"  (colorize "$1" :yellow))
      (str/replace #"(?m)^(\+\+\+\sb.*)$"  (colorize "$1" :yellow))
      (str/replace #"(?m)^(@@.*@@)$"       (colorize "$1" :cyan))
      (str/replace #"(?m)^(\+(?!\+\+).*)$" (colorize "$1" :green))
      (str/replace #"(?m)^(-(?!--).*)$"    (colorize "$1" :red))))

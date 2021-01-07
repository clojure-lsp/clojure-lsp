(ns clojure-lsp.shared
  (:require
   [clojure.string :as string])
  (:import
   [java.net URI]
   [java.nio.file Paths]
   [org.eclipse.lsp4j Range]))

(defn windows-os? []
  (.contains (System/getProperty "os.name") "Windows"))

(defn uri->file-type [uri]
  (cond
    (string/ends-with? uri ".cljs") :cljs
    (string/ends-with? uri ".cljc") :cljc
    (string/ends-with? uri ".clj") :clj
    (string/ends-with? uri ".edn") :edn
    :else :unknown))

(defn uri->path [uri]
  (Paths/get (URI. uri)))

(defn uri->project-related-path [uri project-root]
  (string/replace uri project-root ""))

(defn ->range [{:keys [row end-row col end-col]}]
  {:start {:line (max 0 (dec row)) :character (max 0 (dec col))}
   :end {:line (max 0 (dec end-row)) :character (max 0 (dec end-col))}})

(defn range->clj [^Range range]
  {:start {:line      (.getLine (.getStart range))
           :character (.getCharacter (.getStart range))}
   :end   {:line      (.getLine (.getEnd range))
           :character (.getCharacter (.getEnd range))}})

(defn keywordize-first-depth
  [m]
  (into {}
        (for [[k v] m]
          [(keyword k) v])))

(defn check-bounds
  [line column {:keys [row end-row col end-col] :as _usage}]
  (cond
    (< line row) :before
    (and (= line row) (< column col)) :before
    (< line end-row) :within
    (and (= end-row line) (>= end-col column)) :within
    :else :after))

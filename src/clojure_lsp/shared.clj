(ns clojure-lsp.shared
  (:require
   [clojure.string :as string])
  (:import
   (org.eclipse.lsp4j
     Range)))

(defn uri->file-type [uri]
  (cond
    (string/ends-with? uri ".cljs") :cljs
    (string/ends-with? uri ".cljc") :cljc
    (string/ends-with? uri ".clj") :clj
    (string/ends-with? uri ".edn") :edn
    :else :unknown))

(defn ->range [{:keys [row end-row col end-col]}]
  (cond-> {:start {:line (dec row) :character (dec col)}}
    end-row (assoc :end {:line (dec end-row) :character (dec end-col)})))

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

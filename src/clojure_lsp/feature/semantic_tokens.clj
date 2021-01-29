(ns clojure-lsp.feature.semantic-tokens
  (:require
    [clojure-lsp.db :as db]
    [clojure.string :as string]
    [clojure-lsp.shared :as shared])
  (:import
   [clojure.lang PersistentVector]))

(def token-types
  [:type
   :function
   :macro])

(def token-types-str
  (->> token-types
       (map name)
       vec))

(def token-modifiers
  [])

(def token-modifier -1)

(defn ^:private element-inside-range?
  [{element-row :name-row element-end-row :name-end-row}
   {:keys [name-row name-end-row]}]
  (and (>= element-row name-row)
       (<= element-end-row name-end-row)))

(defn ^:private element->absolute-token
  [{:keys [name-row name-col name-end-col]}
   token-type]
  [(dec name-row)
   (dec name-col)
   (- name-end-col name-col)
   (.indexOf ^PersistentVector token-types token-type)
   token-modifier])

(defn ^:private elements->absolute-tokens
  [elements]
  (->> elements
       (map
         (fn [{:keys [bucket macro] :as element}]
           (cond
            (and macro
                  (= bucket :var-usages))
             [(element->absolute-token element :macro)])))
       (remove nil?)
       (mapcat identity)))

(defn ^:private absolute-token->relative-token
  [tokens
   index
   [row col length token-type token-modifier :as token]]
  (let [[previous-row previous-col _ _ _] (nth tokens (dec index) nil)]
    (cond
      (nil? previous-row)
      token

      (= previous-row row)
      [0
       (- col previous-col)
       length
       token-type
       token-modifier]

      :else
      [(- row previous-row)
       col
       length
       token-type
       token-modifier])))

(defn full-tokens [uri]
  (let [elements (get-in @db/db [:analysis (shared/uri->filename uri)])
        absolute-tokens (elements->absolute-tokens elements)]
    (->> absolute-tokens
         (map-indexed (partial absolute-token->relative-token absolute-tokens))
         flatten)))

(defn range-tokens
  [uri range]
  (let [elements (get-in @db/db [:analysis (shared/uri->filename uri)])
        range-elements (filter #(element-inside-range? % range) elements)
        absolute-tokens (elements->absolute-tokens range-elements)]
    (->> absolute-tokens
         (map-indexed (partial absolute-token->relative-token absolute-tokens))
         flatten)))

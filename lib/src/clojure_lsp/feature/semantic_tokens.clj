(ns clojure-lsp.feature.semantic-tokens
  (:require
   [clojure-lsp.shared :as shared]
   [clojure.string :as string])
  (:import
   [clojure.lang PersistentVector]))

(set! *warn-on-reflection* true)

(def token-types
  [:namespace
   :type
   :function
   :macro
   :keyword
   :class
   :variable
   :method
   :event])

(def token-types-str
  (->> token-types
       (map name)
       vec))

(def token-modifiers
  [:definition
   :defaultLibrary
   :implementation])

(def token-modifiers-str
  (->> token-modifiers
       (map name)
       vec))

(defn ^:private rpad-seq [s n x]
  (take n (concat s (repeat x))))

(defn ^:private token-modifiers->decimal-bit
  [modifiers]
  (as-> token-modifiers $
    (mapv #(if (contains? (set modifiers) %) 1 0) $)
    (rpad-seq $ 8 "0")
    (reverse $)
    (string/join "" $)
    (Integer/parseInt $ 2)))

(defn ^:private decimal-bit->token-modifiers
  [decimal-bit]
  (->> decimal-bit
       Integer/toBinaryString
       reverse
       (map-indexed #(when (= \1 %2) (nth token-modifiers %1)))
       (remove nil?)
       vec))

(defn ^:private element-inside-range?
  [{element-row :name-row element-end-row :name-end-row}
   {:keys [name-row name-end-row]}]
  (and (>= ^Long element-row ^Long name-row)
       (<= ^Long element-end-row ^Long name-end-row)))

(defn ^:private element->absolute-token
  ([element token-type]
   (element->absolute-token element token-type []))
  ([{:keys [name-row name-col name-end-col]}
    token-type
    token-modifier]
   [(dec ^Long name-row)
    (dec ^Long name-col)
    (- ^Long name-end-col ^Long name-col)
    (.indexOf ^PersistentVector token-types token-type)
    (token-modifiers->decimal-bit token-modifier)]))

(defn ^:private java-class-element->absolute-tokens
  [element]
  (cond
    ;; TODO Color only until /
    ;; clj-kondo needs to return more info to handle File/createTempFile and java.io.File/createTempFile
    ;;
    ;; (string/includes? class "/")
    ;; (let [slash (string/index-of class "/")
    ;;       class-pos (assoc element :name-end-col slash)]
    ;;   [(element->absolute-token class-pos :class)])

    :else
    [(element->absolute-token element :class)]))

(defn ^:private var-definition-element->absolute-tokens
  [{:keys [defined-by] :as element}]
  (cond

    defined-by
    [(element->absolute-token element :function [:definition])]

    :else
    nil))

(defn ^:private var-usage-element->absolute-tokens
  [{:keys [name alias macro name-col to] :as element}]
  (let [name-str ^String (str name)]
    (cond
      (and macro
           (not alias))
      [(element->absolute-token element :macro)]

      (and macro
           alias)
      (let [slash (+ name-col (count (str alias)))
            alias-pos (assoc element :name-end-col slash)
            slash-pos (assoc element :name-col slash :name-end-col (inc slash))
            name-pos (assoc element :name-col (inc slash))]
        [(element->absolute-token alias-pos :type)
         (element->absolute-token slash-pos :event)
         (element->absolute-token name-pos :macro)])

      alias
      (let [slash (+ name-col (count (str alias)))
            slash-pos (assoc element :name-col slash :name-end-col (inc slash))
            alias-pos (assoc element :name-end-col slash)
            name-pos (assoc element :name-col (inc slash))]
        [(element->absolute-token alias-pos :type)
         (element->absolute-token slash-pos :event)
         (element->absolute-token name-pos :function)])

      (and (identical? :clj-kondo/unknown-namespace to)
           (.equals \. (.charAt name-str 0)))
      [(element->absolute-token element :method)]

      (identical? :clj-kondo/unknown-namespace to)
      nil

      (and (string/starts-with? name-str "*")
           (string/ends-with? name-str "*")
           (> (count name-str) 2))
      [(element->absolute-token element :variable [:defaultLibrary])]

      :else
      [(element->absolute-token element :function)])))

(defn ^:private keywords->absolute-tokens
  [{:keys [ns alias name-col auto-resolved namespace-from-prefix] :as element}]
  (cond
    (and ns
         (or (not auto-resolved)
             alias)
         (not namespace-from-prefix))
    (let [slash (+ (if alias 2 1) name-col (count (str (or alias ns))))
          ns-pos (assoc element :name-end-col slash)
          slash-pos (assoc element :name-col slash :name-end-col (inc slash))
          name-pos (assoc element :name-col (inc slash))]
      [(element->absolute-token ns-pos :type)
       (element->absolute-token slash-pos :event)
       (element->absolute-token name-pos :keyword)])

    :else
    [(element->absolute-token element :keyword)]))

(defn ^:private elements->absolute-tokens
  [elements]
  (->> elements
       (sort-by (juxt :name-row :name-col))
       (map
         (fn [{:keys [bucket] :as element}]
           (cond
             (contains? #{:java-class-definitions
                          :java-class-usages} bucket)
             (java-class-element->absolute-tokens element)

             (= bucket :var-usages)
             (var-usage-element->absolute-tokens element)

             (= bucket :var-definitions)
             (var-definition-element->absolute-tokens element)

             (#{:locals :local-usages} bucket)
             [(element->absolute-token element :variable)]

             (= bucket :namespace-definitions)
             [(element->absolute-token element :namespace)]

             (and (= bucket :keywords)
                  (not (:str element))
                  (not (:keys-destructuring element)))
             (keywords->absolute-tokens element)

             (= bucket :protocol-impls)
             [(element->absolute-token element :method [:implementation])])))
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

      (= ^Long previous-row ^Long row)
      [0
       (- ^Long col ^Long previous-col)
       length
       token-type
       token-modifier]

      :else
      [(- ^Long row ^Long previous-row)
       col
       length
       token-type
       token-modifier])))

(defn full-tokens [uri db]
  (let [elements (get-in db [:analysis (shared/uri->filename uri)])
        absolute-tokens (elements->absolute-tokens elements)]
    (->> absolute-tokens
         (map-indexed (partial absolute-token->relative-token absolute-tokens))
         flatten
         doall)))

(defn range-tokens
  [uri range db]
  (let [elements (get-in db [:analysis (shared/uri->filename uri)])
        range-elements (filter #(element-inside-range? % range) elements)
        absolute-tokens (elements->absolute-tokens range-elements)]
    (->> absolute-tokens
         (map-indexed (partial absolute-token->relative-token absolute-tokens))
         flatten
         doall)))

(defn element->token-type [element]
  (->> [element]
       elements->absolute-tokens
       (mapv (fn [[_ _ _ type modifier]]
               {:token-type (nth token-types type type)
                :token-modifier (decimal-bit->token-modifiers modifier)}))))

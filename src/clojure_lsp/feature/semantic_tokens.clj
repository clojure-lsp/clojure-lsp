(ns clojure-lsp.feature.semantic-tokens)

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

(defn ^:private usage-inside-range?
  [{usage-row :name-row usage-end-row :name-end-row}
   {:keys [name-row name-end-row]}]
  (and (>= usage-row name-row)
       (<= usage-end-row name-end-row)))

(defn ^:private usage->absolute-token
  [{:keys [name-row name-col name-end-col]}
   token-type]
  [(dec name-row)
   (dec name-col)
   (- name-end-col name-col)
   (.indexOf token-types token-type)
   token-modifier])

(defn ^:private usages->absolute-tokens
  [usages]
  (->> usages
       (map
         (fn [{:keys [bucket macro] :as usage}]
           (cond
            (and macro
                  (= bucket :var-usages))
             [(usage->absolute-token usage :macro)])))
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

(defn full-tokens
  [usages]
  (let [absolute-tokens (usages->absolute-tokens usages)]
    (->> absolute-tokens
         (map-indexed (partial absolute-token->relative-token absolute-tokens))
         flatten)))

(defn range-tokens
  [usages range]
  (let [range-usages (filter #(usage-inside-range? % range) usages)
        absolute-tokens (usages->absolute-tokens range-usages)]
    (->> absolute-tokens
         (map-indexed (partial absolute-token->relative-token absolute-tokens))
         flatten)))

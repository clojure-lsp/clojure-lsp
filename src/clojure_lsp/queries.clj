(ns clojure-lsp.queries
  (:require
    [clojure.tools.logging :as log]
    [clojure.pprint :as pprint]))

(defn ^:private find-first [pred coll]
  (reduce
    (fn [_ i]
      (when (pred i)
        (reduced i)))
    nil
    coll))

(defmulti find-definition
  (fn [_analysis element]
    (:bucket element)))

(defmethod find-definition :var-usages
  [analysis element]
  (find-first #(and (= (:bucket %) :var-definitions)
                    (= (:name %) (:name element))
                    (= (:ns %) (:to element)))
              (mapcat val analysis)))

(defmethod find-definition :local-usages
  [analysis {:keys [id filename] :as _element}]
  (find-first #(and (= :locals (:bucket %)) (= (:id %) id))
              (get analysis filename)))

(defmethod find-definition :default
  [_analysis element]
  element)

(defmulti find-references
  (fn [_analysis element _include-declaration?]
    (case (:bucket element)
      :locals :local
      :local-usages :local
      (:bucket element))))

(defmethod find-references :var-usages
  [analysis element _]
  (filter #(and (= (:name %) (:name element))
                (= (or (:ns %) (:to %)) (:to element)))
          (mapcat val analysis)))

(defmethod find-references :var-definitions
  [analysis element include-declaration?]
  (if include-declaration?
    (filter #(and (= (:name %) (:name element))
                  (= (or (:ns %) (:to %)) (:ns element)))
            (mapcat val analysis))
    (filter #(and (= (:name %) (:name element))
                  (= (:to %) (:ns element)))
            (mapcat val analysis))))

(defmethod find-references :local
  [analysis {:keys [id filename] :as _element} _]
  (filter #(= (:id %) id) (get analysis filename)))

(defmethod find-references :default
  [_analysis element _]
  [element])

(defn find-element-under-cursor
  [analysis filename line column]
  (let [local-analysis (get analysis filename)]
    (find-first (fn [{:keys [name-row name-col name-end-row name-end-col] :as _v}]
                  (and (<= name-row line name-end-row)
                       (<= name-col column name-end-col)))
                local-analysis)))

(defn find-definition-from-cursor [analysis filename line column]
  (try
    (let [element (find-element-under-cursor analysis filename line column)]
      (when element
        (find-definition analysis element)))
    (catch Throwable e
      (log/error e "can't find definition"))))

(defn find-references-from-cursor [analysis filename line column include-declaration?]
  (try
    (let [element (find-element-under-cursor analysis filename line column)]
      (when element
        (find-references analysis element include-declaration?)))
    (catch Throwable e
      (log/error e "can't find references"))))

(defn find-vars [analysis filename include-private?]
  (filter #(and (= (:bucket %) :var-definitions)
                (or include-private?
                    (not (get % :private))))
          (get analysis filename)))

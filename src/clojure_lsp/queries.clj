(ns clojure-lsp.queries
  (:require
    [clojure.tools.logging :as log]
    [medley.core :as medley]))

(defn ^:private find-first [pred coll]
  (reduce
    (fn [_ i]
      (when (pred i)
        (reduced i)))
    nil
    coll))

(defmulti find-definition
  (fn [analysis element]
    (:bucket element)))

(defmethod find-definition :var-usages
  [analysis element]
  (find-first #(and (= (:bucket %) :var-definitions)
                    (= (:name %) (:name element))
                    (= (:ns %) (:to element)))
              (mapcat val analysis)))

(defmethod find-definition :local-usages
  [analysis {:keys [id filename] :as element}]
  (find-first #(and (= :locals (:bucket %)) (= (:id %) id))
              (get analysis filename)))

(defmethod find-definition :default
  [_analysis element]
  element)

(defmulti find-references
  (fn [analysis element]
    (case (:bucket element)
      :locals :local
      :local-usages :local
      (:bucket element))))

(defmethod find-references :var-usages
  [analysis element]
  (filter #(and (= (:name %) (:name element))
                (= (or (:ns %) (:to %)) (:to element)))
          (mapcat val analysis)))

(defmethod find-references :var-definitions
  [analysis element]
  (filter #(and (= (:name %) (:name element))
                       (= (or (:ns %) (:to %)) (:ns element)))
                 (mapcat val analysis)))

(defmethod find-references :local
  [analysis {:keys [id filename] :as element}]
  (filter #(= (:id %) id) (get analysis filename)))

(defmethod find-references :default
  [_analysis element]
  [element])

(defn find-element-under-cursor
  [analysis filename line column]
  (find-first (fn [{:keys [name-row name-col name-end-row name-end-col] :as v}]
                (and (<= name-row line name-end-row)
                     (<= name-col column name-end-col)))
              (get analysis filename)))

(defn find-definition-from-cursor [analysis filename line column]
  (try
    (let [{:keys [bucket] :as element} (find-element-under-cursor analysis filename line column)]
      (when (log/spy element)
        (find-definition analysis element)))
    (catch Throwable e
      (log/error e "can't find definition"))))

(defn find-references-from-cursor [analysis filename line column]
  (try
    (let [
          {:keys [bucket] :as element} (find-element-under-cursor analysis filename line column)]
      (when element
        (find-references analysis element)))
   (catch Throwable e
      (log/error e "can't find references"))))

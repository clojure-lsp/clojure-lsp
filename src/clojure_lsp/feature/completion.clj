(ns clojure-lsp.feature.completion
  (:require
    [clojure-lsp.clojure-core :as cc]
    [clojure-lsp.db :as db]
    [clojure-lsp.feature.hover :as f.hover]
    [clojure-lsp.parser :as parser]
    [clojure-lsp.queries :as q]
    [clojure-lsp.shared :as shared]
    [clojure.string :as string]
    [clojure.tools.logging :as log]
    [rewrite-clj.zip :as z]))

(defn ^:private remove-keys [pred m]
  (apply dissoc m (filter pred (keys m))))

(defn ^:private matches-cursor? [cursor-value s]
  (when (and s (string/starts-with? s cursor-value))
    s))

;; TODO improve to use better kinds
(defn element->completion-item-kind [{:keys [bucket]}]
  (cond
    (#{:namespace-definitions
       :namespace-usages} bucket)
    :module

    (#{:var-definitions} bucket)
    :function

    (#{:var-usages} bucket)
    :variable

    (#{:locals :localusages} bucket)
    :value

    :else
    :text))

(defn completion [uri row col]
  (let [filename (shared/uri->filename uri)
        file-type (shared/uri->file-type uri)
        consider-cljs? (#{:cljc :cljs} file-type)
        consider-core? (#{:cljc :clj} file-type)
        {:keys [text]} (get-in @db/db [:documents uri])
        analysis (get @db/db :analysis)
        ns-elements (get analysis filename)
        other-ns-elements (->> (dissoc analysis filename)
                               (remove-keys #(not (string/starts-with? (-> % name shared/filename->uri) "file://")))
                               (mapcat val))
        cursor-loc (try
                     (parser/loc-at-pos text row (dec col))
                     (catch Exception e
                       (log/error (.getMessage e) "It was not possible to find cursor location for completion")))
        cursor-element (loop [try-column col]
                         (if-let [usage (first (q/find-references-from-cursor (:analysis @db/db) filename row col true))]
                           usage
                           (when (pos? try-column)
                             (recur (dec try-column)))))
        cursor-value (if cursor-loc
                       (z/sexpr cursor-loc)
                       (:name cursor-element))
        matches-fn (partial matches-cursor? cursor-value)]
    (concat
      (->> ns-elements
           (filter #(and (not (#{:var-usages :local-usages} (:bucket %)))
                         (matches-fn (:name %))))
           (remove #(= :clj-kondo/unknown-namespace (:to %)))
           (map (fn [element]
                  {:label  (-> element :name name)
                   :kind   (element->completion-item-kind element)
                   :tags   (when (:deprecated element)
                             [1])
                   :data element
                   :detail (some-> element :ns name)}))
           (sort-by :label))
      (->> other-ns-elements
           (filter #(and (not (#{:var-usages :local-usages} (:bucket %)))
                         (matches-fn (:name %))))
           (remove #(= :clj-kondo/unknown-namespace (:to %)))
           (map (fn [element]
                  {:label  (-> element :name name)
                   :kind   (element->completion-item-kind element)
                   :tags   (when (:deprecated element)
                             [1])
                   :data element
                   :detail (some-> element :ns name)}))
           (sort-by :label))
      (->> cc/core-syms
           (filter (comp matches-fn str))
           (map (fn [sym] {:label (str sym)
                           :detail "clojure.core"}))
           (sort-by :label))
      (when consider-cljs?
        (->> cc/cljs-syms
             (filter (comp matches-fn str))
             (map (fn [sym] {:label (str sym)
                             :detail "cljs.core"}))
             (sort-by :label)))
      (when consider-core?
        (concat
          (->> cc/java-lang-syms
               (filter (comp matches-fn str))
               (map (fn [sym] {:label  (str sym)
                               :detail "java.lang"}))
               (sort-by :label))
          (->> cc/java-util-syms
               (filter (comp matches-fn str))
               (map (fn [sym] {:label  (str sym)
                               :detail "java.util"}))
               (sort-by :label)))))))

(defn resolve-item [{:keys [data] :as item}]
  (if data
    (assoc item :documentation (f.hover/hover-documentation data))
    item))

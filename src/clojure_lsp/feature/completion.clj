(ns clojure-lsp.feature.completion
  (:require
    [clojure-lsp.clojure-core :as cc]
    [clojure-lsp.db :as db]
    [clojure-lsp.feature.hover :as f.hover]
    [clojure-lsp.parser :as parser]
    [clojure-lsp.queries :as q]
    [clojure-lsp.shared :as shared]
    [clojure.string :as string]
    [taoensso.timbre :as log]
    [clojure.walk :as walk]
    [rewrite-clj.zip :as z]))

(defn ^:private remove-keys [pred m]
  (apply dissoc m (filter pred (keys m))))

(defn ^:private matches-cursor? [cursor-value s]
  (when (and s
             cursor-value
             (string/starts-with? s (name cursor-value)))
    s))

(defn ^:private supports-cljs? [uri]
  (#{:cljc :cljs} (shared/uri->file-type uri)))

(defn ^:private supports-clj-core? [uri]
  (#{:cljc :clj} (shared/uri->file-type uri)))

(defn element->completion-item-kind [{:keys [bucket fixed-arities]}]
  (cond
    (#{:namespace-definitions
       :namespace-usages
       :namespace-alias} bucket)
    :module

    (and (#{:var-definitions} bucket)
         fixed-arities)
    :function

    (#{:var-definitions :var-usages} bucket)
    :variable

    (#{:locals :localusages} bucket)
    :value

    :else
    :text))

(defn element->completion-item [{:keys [deprecated alias ns bucket arglist-strs] :as element}]
  (let [kind (element->completion-item-kind element)
        detail (or (when arglist-strs (string/join " " arglist-strs))
                   (some-> ns name))
        definition? (#{:namespace-definitions :var-definitions} bucket)]
    (-> {:label  (or (some-> alias name)
                     (-> element :name name))}
        (cond-> detail (assoc :detail detail)
                deprecated (assoc :tags [1])
                kind (assoc :kind kind)
                definition? (assoc :data (-> element
                                             (select-keys [:ns :name :doc :filename :arglist-strs])
                                             walk/stringify-keys))))))

(defn valid-element-completion-item?
  [matches-fn
   cursor-uri
   {cursor-from :from cursor-bucket :bucket :as _cursor-element}
   {:keys [bucket to ns filename lang name alias] :as _element}]
  (let [supported-file-types #{:cljc (shared/uri->file-type cursor-uri)}]
    (cond
      (#{:var-usages :local-usages :namespace-usages} bucket)
      false

      (and (= bucket :locals)
           (not= filename (shared/uri->filename cursor-uri)))
      false

      (and (= bucket :var-definitions)
           (= cursor-bucket :var-usages)
           (not= ns cursor-from))
      false

      (= :clj-kondo/unknown-namespace to)
      false

      (and lang
           (not (supported-file-types lang)))
      false

      (or (and name (matches-fn name))
          (and alias (matches-fn alias)))
      true)))

(defn with-element-items [elements matches-fn cursor-uri cursor-element]
  (->> elements
       (filter (partial valid-element-completion-item? matches-fn cursor-uri cursor-element))
       (map element->completion-item)
       (sort-by :label)))

(defn with-elements-from-alias [alias matches-fn ns-elements other-elements]
  (when-let [alias-ns (some->> ns-elements
                               (q/find-first #(and (= (:bucket %) :namespace-usages)
                                                   (= (-> % :alias str) alias)))
                               :name)]
    (->> other-elements
         (filter #(and (= (:bucket %) :var-definitions)
                       (= (:ns %) alias-ns)
                       (matches-fn (:name %))))
         (map element->completion-item)
         (sort-by :label))))

(defn with-clojure-core-items [matches-fn]
  (->> cc/core-syms
       (filter (comp matches-fn str))
       (map (fn [sym] {:label (str sym)
                       :detail "clojure.core"}))
       (sort-by :label)))

(defn with-clojurescript-items [matches-fn]
  (->> cc/cljs-syms
       (filter (comp matches-fn str))
       (map (fn [sym] {:label (str sym)
                       :detail "cljs.core"}))
       (sort-by :label)))

(defn with-java-items [matches-fn]
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
         (sort-by :label))))

(defn completion [uri row col]
  (let [filename (shared/uri->filename uri)
        {:keys [text]} (get-in @db/db [:documents uri])
        analysis (get @db/db :analysis)
        current-ns-elements (get analysis filename)
        other-ns-elements (->> (dissoc analysis filename)
                               (remove-keys #(not (string/starts-with? (-> % name shared/filename->uri) "file://")))
                               (mapcat val))
        external-ns-elements (->> (dissoc analysis filename)
                                  (remove-keys #(string/starts-with? (-> % name shared/filename->uri) "file://"))
                                  (mapcat val))
        cursor-loc     (parser/safe-loc-at-pos text row col)
        cursor-element (loop [try-column col]
                         (if-let [usage (q/find-element-under-cursor analysis filename row col)]
                           usage
                           (when (pos? try-column)
                             (recur (dec try-column)))))
        cursor-value (if cursor-loc
                       (z/sexpr cursor-loc)
                       (:name cursor-element))
        matches-fn (partial matches-cursor? cursor-value)
        cursor-alias (some-> cursor-loc z/sexpr namespace)]
    (cond-> []

      cursor-alias
      (concat (with-elements-from-alias cursor-alias matches-fn current-ns-elements (concat other-ns-elements
                                                                                            external-ns-elements)))

      (not cursor-alias)
      (concat (with-element-items current-ns-elements matches-fn uri cursor-element)
              (with-element-items other-ns-elements matches-fn uri cursor-element)
              (with-clojure-core-items matches-fn))

      (and (not cursor-alias)
           (supports-cljs? uri))
      (concat (with-clojurescript-items matches-fn))

      (and (not cursor-alias)
           (supports-clj-core? uri))
      (concat (with-java-items matches-fn)))))

(defn resolve-item [{:keys [kind data] :as item}]
  (->
    (if data
      (-> item
          (assoc :documentation (f.hover/hover-documentation data))
          (dissoc :data))
      item)
    (assoc :kind (some-> kind .toLowerCase keyword))))

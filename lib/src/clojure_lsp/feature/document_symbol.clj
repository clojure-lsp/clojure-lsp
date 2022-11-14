(ns clojure-lsp.feature.document-symbol
  (:require
   [clojure-lsp.queries :as q]
   [clojure-lsp.shared :as shared]))

(set! *warn-on-reflection* true)

(def ^:private defines-interface?
  '#{clojure.core/defprotocol cljs.core/defprotocol
     clojure.core/definterface cljs.core/definterface
     clojure.core/defmulti cljs.core/defmulti})

(def ^:private defines-class?
  '#{clojure.core/defrecord cljs.core/defrecord
     clojure.core/deftype cljs.core/deftype})

(defn element->symbol-kind [{:keys [bucket] :as el}]
  (case bucket
    (:namespace-usages :namespace-definitions) :namespace
    :var-definitions (cond
                       (or (:fixed-arities el) (:varargs-min-arity el) (:macro el))
                       #_=> :function
                       (some->> el :defined-by defines-interface?)
                       #_=> :interface
                       (some->> el :defined-by defines-class?)
                       #_=> :class
                       :else
                       #_=> :variable)
    :var-usages (if (:defmethod el)
                  :function
                  :variable)
    :null))

(defn element->name [{elem-name :name :keys [dispatch-val-str]}]
  (cond-> (name elem-name)
    dispatch-val-str (str " " dispatch-val-str)))

(defn ^:private element->document-symbol [e]
  (shared/assoc-some
    {:name (element->name e)
     :kind (element->symbol-kind e)
     :range (shared/->scope-range e)
     :selection-range (shared/->range e)
     :tags (cond-> []
             (:deprecated e) (conj 1))}
    :detail (when (:private e)
              "private")))

(defn ^:private symbol-order [{:keys [selection-range]}]
  [(:line (:start selection-range)) (:character (:start selection-range))])

(defn document-symbols [db uri]
  (let [namespace-definition (q/find-namespace-definition-by-uri db uri)]
    [{:name (or (some-> namespace-definition :name name)
                ;; TODO Consider using URI for display purposes, especially if
                ;; we support remote LSP connections
                (shared/uri->filename uri))
      :kind (element->symbol-kind namespace-definition)
      :range shared/full-file-range
      :selection-range (if namespace-definition
                         (shared/->scope-range namespace-definition)
                         shared/full-file-range)
      :children (->> (concat (q/find-var-definitions db uri true)
                             (q/find-defmethods db uri))
                     (map element->document-symbol)
                     (sort-by symbol-order)
                     vec)}]))

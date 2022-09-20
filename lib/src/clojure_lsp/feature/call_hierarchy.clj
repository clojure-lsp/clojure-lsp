(ns clojure-lsp.feature.call-hierarchy
  (:require
   [clojure-lsp.feature.document-symbol :as f.document-symbol]
   [clojure-lsp.feature.file-management :as f.file-management]
   [clojure-lsp.parser :as parser]
   [clojure-lsp.queries :as q]
   [clojure-lsp.refactor.edit :as edit]
   [clojure-lsp.shared :as shared]
   [clojure.string :as string]
   [rewrite-clj.zip :as z]))

(set! *warn-on-reflection* true)

(defn ^:private safe-zloc-of-forced-uri [components uri]
  (some-> (f.file-management/force-get-document-text uri components)
          (parser/safe-zloc-of-string)))

(defn ^:private element-by-uri->call-hierarchy-item
  [{:keys [uri parent-element usage-element]}]
  (let [{:keys [ns arglist-strs deprecated] el-uri :uri el-name :name} parent-element]
    {:name (if arglist-strs
             (str (name el-name) " " (some->> arglist-strs (remove nil?) (string/join " ")))
             (name el-name))
     :kind (f.document-symbol/element->symbol-kind parent-element)
     :tags (cond-> [] deprecated (conj 1))
     ;; TODO Consider using URI for display purposes, especially if we support
     ;; remote LSP connections
     :detail (or (some-> ns str) (shared/uri->filename el-uri))
     :uri uri
     :range (shared/->range parent-element)
     :selection-range (shared/->range usage-element)}))

(defn prepare [uri row col db*]
  (let [db @db*
        cursor-element (q/find-element-under-cursor db uri row col)]
    [(element-by-uri->call-hierarchy-item
       {:uri uri
        :usage-element cursor-element
        :parent-element cursor-element})]))

(defn ^:private parent-var-def
  "Returns var def analysis element that surrounds the element at row/col (or
  that is the element at that position)."
  [root-zloc db uri row col]
  (when-let [{:keys [row col]} (some-> root-zloc
                                       (parser/to-pos row col)
                                       (edit/find-var-definition-name-loc)
                                       z/node
                                       meta)]
    (when-let [definition (q/find-element-under-cursor db uri row col)]
      (when (or (identical? :var-definitions (:bucket definition))
                (and (identical? :var-usages (:bucket definition))
                     (:defmethod definition)))
        definition))))

(defn ^:private element->incoming-usage-by-uri
  [db root-zloc {:keys [name-row name-col uri] :as element}]
  (when-let [parent-element (parent-var-def root-zloc db uri name-row name-col)]
    {:uri uri
     :usage-element element
     :parent-element parent-element}))

(defn ^:private element->outgoing-usage-by-uri
  [db element]
  (when-let [definition (q/find-definition db element)]
    {:uri (:uri definition)
     :usage-element element
     :parent-element definition}))

(defn incoming [uri row col {:keys [db*] :as components}]
  (let [db @db*
        references (q/find-references-from-cursor db uri row col false)
        uris (->> references (map :uri) distinct)
        root-zlocs (zipmap uris
                           (map #(safe-zloc-of-forced-uri components %) uris))
        db @db*]
    (->> references
         (keep (fn [element]
                 (element->incoming-usage-by-uri db (get root-zlocs (:uri element)) element)))
         (mapv (fn [element-by-uri]
                 {:from-ranges []
                  :from (element-by-uri->call-hierarchy-item element-by-uri)})))))

(defn outgoing [uri row col {:keys [db*] :as components}]
  (let [root-zloc (safe-zloc-of-forced-uri components uri)
        db @db*]
    (when-let [definition (parent-var-def root-zloc db uri row col)]
      (->> (q/find-var-usages-under-form db
                                         uri
                                         {:row (:name-row definition)
                                          :col (:name-col definition)
                                          :end-row (:end-row definition)
                                          :end-col (:end-col definition)})
           (keep (partial element->outgoing-usage-by-uri db))
           (mapv (fn [element-by-uri]
                   {:from-ranges []
                    :to (element-by-uri->call-hierarchy-item element-by-uri)}))))))

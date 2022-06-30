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

(defn ^:private safe-zloc-of-forced-uri [db* uri]
  (some-> (f.file-management/force-get-document-text uri db*)
          (parser/safe-zloc-of-string)))

;; TODO: call-hierarchies could be made much faster if
;; element-by-uri->call-hierarchy-item didn't re-parse so often.
;; 1. Reason through whether it really needs the branch that parses. The tests
;;    suggest it doesn't.
;; 2. Failing that, when dep-graph is merged, use it to find ns name.
;; 3. Failing that, gather URIs that need to be parsed up front, parse each one
;;    once, then thread those root zlocs through the rest of the code.
(defn ^:private element-by-uri->call-hierarchy-item
  [{:keys [uri parent-element usage-element]} db*]
  (let [{:keys [ns filename name-row name-col arglist-strs deprecated] el-name :name} parent-element
        project-file? (string/starts-with? uri "file://")
        detail (if project-file?
                 ;; TODO: tests pass if this branch is removed. Why can't we use
                 ;; the else branch for project files?
                 (some-> (safe-zloc-of-forced-uri db* uri)
                         (parser/to-pos name-row name-col)
                         edit/find-namespace-name)
                 (or (some-> ns str)
                     filename))]
    {:name (if arglist-strs
             (str (name el-name) " " (some->> arglist-strs (remove nil?) (string/join " ")))
             (name el-name))
     :kind (f.document-symbol/element->symbol-kind parent-element)
     :tags (cond-> [] deprecated (conj 1))
     :detail detail
     :uri uri
     :range (shared/->range parent-element)
     :selection-range (shared/->range usage-element)}))

(defn prepare [uri row col db*]
  (let [db @db*
        cursor-element (q/find-element-under-cursor db (shared/uri->filename uri) row col)]
    [(element-by-uri->call-hierarchy-item
       {:uri uri
        :usage-element cursor-element
        :parent-element cursor-element}
       db*)]))

(defn ^:private parent-var-def
  "Returns var def analysis element that surrounds the element at row/col (or
  that is the element at that position)."
  [root-zloc db filename row col]
  (when-let [{:keys [row col]} (some-> root-zloc
                                       (parser/to-pos row col)
                                       (edit/find-var-definition-name-loc)
                                       z/node
                                       meta)]
    (when-let [definition (q/find-element-under-cursor db filename row col)]
      (when (or (identical? :var-definitions (:bucket definition))
                (and (identical? :var-usages (:bucket definition))
                     (:defmethod definition)))
        definition))))

(defn ^:private element->incoming-usage-by-uri
  [db {:keys [uri root-zloc]} {:keys [name-row name-col filename] :as element}]
  (when-let [parent-element (parent-var-def root-zloc db filename name-row name-col)]
    {:uri uri
     :usage-element element
     :parent-element parent-element}))

(defn ^:private element->outgoing-usage-by-uri
  [db element]
  (when-let [definition (q/find-definition db element)]
    (let [def-filename (:filename definition)
          definition-uri (if (shared/plain-uri? def-filename)
                           def-filename
                           (shared/filename->uri def-filename db))]
      {:uri definition-uri
       :usage-element element
       :parent-element definition})))

(defn incoming [uri row col db*]
  (let [db @db*
        references (q/find-references-from-cursor db (shared/uri->filename uri) row col false)
        filenames (->> references (map :filename) distinct)
        file-meta (zipmap filenames
                          (map (fn [filename]
                                 (let [uri (shared/filename->uri filename db)]
                                   {:uri uri
                                    :root-zloc (safe-zloc-of-forced-uri db* uri)}))
                               filenames))
        db @db*]
    (->> references
         (keep (fn [element]
                 (element->incoming-usage-by-uri db (get file-meta (:filename element)) element)))
         (mapv (fn [element-by-uri]
                 {:from-ranges []
                  :from (element-by-uri->call-hierarchy-item element-by-uri db*)})))))

(defn outgoing [uri row col db*]
  (let [filename (shared/uri->filename uri)
        root-zloc (safe-zloc-of-forced-uri db* uri)
        db @db*]
    (when-let [definition (parent-var-def root-zloc db filename row col)]
      (->> (q/find-var-usages-under-form db
                                         filename
                                         (:name-row definition)
                                         (:name-col definition)
                                         (:end-row definition)
                                         (:end-col definition))
           (keep (partial element->outgoing-usage-by-uri db))
           (mapv (fn [element-by-uri]
                   {:from-ranges []
                    :to (element-by-uri->call-hierarchy-item element-by-uri db*)}))))))

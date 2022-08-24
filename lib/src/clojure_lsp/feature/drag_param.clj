(ns clojure-lsp.feature.drag-param
  (:require
   [clojure-lsp.feature.clauses :as f.clauses]
   [clojure-lsp.feature.drag :as f.drag]
   [clojure-lsp.feature.file-management :as f.file-management]
   [clojure-lsp.parser :as parser]
   [clojure-lsp.queries :as q]
   [clojure-lsp.refactor.edit :as edit]
   [clojure-lsp.shared :as shared]
   [medley.core :as medley]
   [rewrite-clj.paredit :as paredit]
   [rewrite-clj.zip :as z]))

(def ^:private param-establishing-symbols
  '#{defn defn- defmacro})

(defn z-safe-sexpr [zloc]
  (when (z/sexpr-able? zloc)
    (z/sexpr zloc)))

(defn ^:private plan [zloc dir uri db]
  ;; TODO: handle quote, etc here
  (let [vec-zloc (z/up zloc)]
    ;; exclude maps, sets, lists, calls, etc
    (when (z/vector? vec-zloc)
      ;; exclude non-param vectors and multi-arity functions
      (when (some-> vec-zloc z/leftmost z-safe-sexpr param-establishing-symbols)
        ;; TODO: re-use marker to find origin-clause
        (let [zloc (edit/mark-position zloc ::orig)]
          ;; exclude vararg, both from dragging and from pulp clauses
          (when-let [zloc (if-let [vararg-marker-loc (some-> zloc
                                                             z/leftmost
                                                             (z/find-value '&))]
                            (-> vararg-marker-loc
                                paredit/kill ;; remove to right of &
                                z/remove ;; remove &
                                z/leftmost ;; start returning to original node
                                (z/find f.clauses/z-right #(edit/marked? % ::orig)))
                            zloc)]
            (let [{:keys [zloc] :as clause-spec} (f.clauses/clause-spec zloc uri db)]
              (when (f.drag/probable-valid-movement? zloc dir clause-spec)
                clause-spec))))))))

(defn ^:private usage-edits [zloc dir clause-idx uri {:keys [db db*] :as components}]
  (let [{:keys [row col]} (meta (z/node (edit/find-var-definition-name-loc zloc)))
        elem (q/find-element-under-cursor db (shared/uri->filename uri) row col)]
    (when (= :var-definitions (:bucket elem))
      (let [var-usages (q/find-references db elem false)]
        (->> var-usages
             (group-by :filename)
             (medley/map-kv (fn [filename var-usages]
                              (let [uri (shared/filename->uri filename db)]
                                (when-let [usage-text (f.file-management/force-get-document-text uri components)]
                                  (let [db @db*
                                        root-zloc (parser/safe-zloc-of-string usage-text)]
                                    [uri (mapcat (fn [{:keys [name-row name-col]}]
                                                   (let [var-usage-loc (parser/to-pos root-zloc name-row name-col)
                                                         arg-loc (->> (f.clauses/z-right var-usage-loc)
                                                                      (iterate f.clauses/z-right)
                                                                      (take-while identity)
                                                                      (drop clause-idx)
                                                                      first)]
                                                     (some-> arg-loc
                                                             (f.clauses/clause-spec uri db)
                                                             (f.drag/identify-clauses)
                                                             (f.drag/nodes-to-drag dir)
                                                             (f.drag/node-edits))))
                                                 var-usages)]))))))))))

;;;; Public API

;; Drag param at zloc forward or backward.

(defn ^:private can-drag? [zloc dir uri db] (boolean (plan zloc dir uri db)))
(defn can-drag-backward? [zloc uri db] (can-drag? zloc :backward uri db))

(defn can-drag-forward? [zloc uri db] (can-drag? zloc :forward uri db))

(defn ^:private drag [zloc dir cursor-position uri {:keys [db*] :as components}]
  (let [db @db*]
    (when-let [clause-spec (plan zloc dir uri db)]
      (when-let [clause-data (f.drag/identify-clauses clause-spec)]
        (when-let [[edits cursor-position] (f.drag/drag-clause clause-data dir cursor-position)]
          {:show-document-after-edit {:uri        uri
                                      :take-focus true
                                      :range      cursor-position}
           :changes-by-uri (shared/deep-merge {uri edits}
                                              (usage-edits (:zloc clause-spec)
                                                           dir
                                                           (:idx (:origin-clause clause-data))
                                                           uri
                                                           (assoc components :db db)))})))))

(defn drag-backward [zloc cursor-position uri components] (drag zloc :backward cursor-position uri components))
(defn drag-forward [zloc cursor-position uri components] (drag zloc :forward cursor-position uri components))

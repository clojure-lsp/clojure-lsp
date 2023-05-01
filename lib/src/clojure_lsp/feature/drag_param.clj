(ns clojure-lsp.feature.drag-param
  (:require
   [clojure-lsp.feature.clauses :as f.clauses]
   [clojure-lsp.feature.drag :as f.drag]
   [clojure-lsp.feature.file-management :as f.file-management]
   [clojure-lsp.parser :as parser]
   [clojure-lsp.producer :as producer]
   [clojure-lsp.queries :as q]
   [clojure-lsp.refactor.edit :as edit]
   [clojure-lsp.shared :as shared]
   [medley.core :as medley]
   [rewrite-clj.paredit :as paredit]
   [rewrite-clj.zip :as z]))

(def ^:private param-establishing-symbols
  '#{defn defn- defmacro})

(defn ^:private plan [zloc dir uri db]
  (let [vec-zloc (z/up zloc)]
    ;; exclude maps, sets, lists, calls, etc
    (when (z/vector? vec-zloc)
      ;; exclude non-param vectors and multi-arity functions
      (when (some-> vec-zloc z/leftmost parser/safe-zloc-sexpr param-establishing-symbols)
        (let [zloc (edit/mark-position zloc ::orig)]
          ;; exclude vararg, both from dragging and from pulp clauses
          (when-let [zloc (if-let [vararg-marker-loc (some-> zloc
                                                             z/leftmost
                                                             (z/find-value '&))]
                            (-> vararg-marker-loc
                                paredit/kill ;; remove to right of &
                                z/remove     ;; remove &
                                z/leftmost   ;; start returning to original node
                                (z/find z/right* #(edit/marked? % ::orig)))
                            zloc)]
            (f.drag/plan zloc dir uri db)))))))

(defn ^:private usage-edit [root-zloc clause-idx uri db dir {:keys [name-row name-col]}]
  (let [var-usage-loc (parser/to-pos root-zloc name-row name-col)]
    ;; exclude locations where usage is not in function-call position
    (if (and (z/list? (f.clauses/z-up var-usage-loc))
             (or (f.clauses/z-leftmost? var-usage-loc)
                 (let [left-loc (f.clauses/z-left var-usage-loc)] ;; except for `(partial f ...)` which we can sometimes handle.
                   (and (f.clauses/z-leftmost? left-loc)
                        (= 'partial (parser/safe-zloc-sexpr left-loc))))))
      (let [arg-loc (->> (f.clauses/z-right var-usage-loc)
                         (iterate f.clauses/z-right)
                         (take-while identity)
                         (drop clause-idx)
                         first)
            clause-spec (some-> arg-loc (f.clauses/clause-spec uri db))]
        (if (:in-threading? clause-spec)
          {:skipped? true}
          (let [edits (some-> clause-spec
                              (f.drag/identify-clauses)
                              (f.drag/nodes-to-drag dir)
                              (f.drag/node-edits))]
            (if (seq edits)
              {:edits edits}
              {:skipped? true}))))
      {:skipped? true})))

(defn ^:private usage-edits [zloc dir clause-idx uri {:keys [db db*] :as components}]
  (let [{:keys [row col]} (meta (z/node (edit/find-var-definition-name-loc zloc)))
        elem (q/find-element-under-cursor db uri row col)]
    (when (= :var-definitions (:bucket elem))
      (let [var-usages (q/find-references db elem false)
            edits-by-uri (->> var-usages
                              (group-by :uri)
                              (medley/map-kv (fn [uri var-usages]
                                               (when-let [usage-text (f.file-management/force-get-document-text uri components)]
                                                 (let [db @db*
                                                       root-zloc (parser/safe-zloc-of-string usage-text)
                                                       usage-edits (map #(usage-edit root-zloc clause-idx uri db dir %1)
                                                                        var-usages)
                                                       skipped-any? (some :skipped? usage-edits)
                                                       edits (mapcat :edits usage-edits)]
                                                   [uri {:skipped? skipped-any?
                                                         :edits edits}])))))]
        [(->> edits-by-uri
              (keep (fn [[uri {:keys [edits]}]]
                      (when (seq edits)
                        [uri edits])))
              (into {}))
         (some :skipped? (vals edits-by-uri))]))))

;;;; Public API

;; Drag param at zloc forward or backward.

(defn ^:private can-drag? [zloc dir uri db] (boolean (plan zloc dir uri db)))

(defn can-drag-backward? [zloc uri db] (can-drag? zloc :backward uri db))
(defn can-drag-forward? [zloc uri db] (can-drag? zloc :forward uri db))

(defn ^:private warn-skipped-usages [producer]
  (producer/show-message producer
                         "Cannot drag. Call sites include ->, ->>, partial, apply, or certain other forms which cannot be safely refactored."
                         :error
                         nil))

(defn ^:private drag [zloc dir cursor-position uri {:keys [db* producer] :as components}]
  (let [db @db*]
    (when-let [clause-spec (plan zloc dir uri db)]
      (when-let [clause-data (f.drag/identify-clauses clause-spec)]
        (when-let [defn-edits (f.drag/drag-clause clause-data dir cursor-position uri)]
          (let [[usage-edits usages-skipped?]
                (usage-edits (:zloc clause-spec)
                             dir
                             (:idx (:origin-clause clause-data))
                             uri
                             (assoc components :db db))]
            (if usages-skipped?
              (do (warn-skipped-usages producer)
                  nil)
              (update defn-edits :changes-by-uri shared/deep-merge usage-edits))))))))

(defn drag-backward [zloc cursor-position uri components] (drag zloc :backward cursor-position uri components))
(defn drag-forward [zloc cursor-position uri components] (drag zloc :forward cursor-position uri components))

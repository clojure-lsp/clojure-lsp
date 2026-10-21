(ns clojure-lsp.feature.move-form
  (:require
   [clojure-lsp.dep-graph :as dep-graph]
   [clojure-lsp.feature.add-missing-libspec :as f.add-missing-libspec]
   [clojure-lsp.feature.file-management :as f.file-management]
   [clojure-lsp.parser :as parser]
   [clojure-lsp.queries :as q]
   [clojure-lsp.refactor.edit :as edit]
   [clojure-lsp.refactor.transform :as r.transform]
   [clojure-lsp.shared :as shared]
   [medley.core :as medley]
   [rewrite-clj.zip :as z]))

(defn drop-ns-from-dest [zloc usages]
  (loop [[usage & other-usages] usages
         loc zloc]
    (if usage
      (let [usage-loc (edit/find-at-element-name loc usage)]
        (recur other-usages (z/subedit-> loc
                                         (edit/find-at-element-name usage)
                                         (z/replace (symbol (name (z/sexpr usage-loc)))))))
      loc)))

(defn var-usages-within [zloc uri db]
  (let [scope (meta (z/node zloc))
        defs (q/find-var-usages-under-form db uri scope)]
    (filterv
      #(edit/loc-encapsulates-usage? zloc %)
      defs)))

(defn var-definitions-within [zloc uri db]
  (let [defs (q/find-var-definitions db uri false)]
    (filterv
      #(edit/loc-encapsulates-usage? zloc %)
      defs)))

(defn ^:private find-source-refer
  "The `:refer` of the moved var in the ns form of a file, if any."
  [local-buckets source-ns def-to-move]
  (first (filter #(and (:refer %)
                       (= (:to %) source-ns)
                       (= (:name %) (:name def-to-move)))
                 (:var-usages local-buckets))))

(defn ^:private find-other-source-usages
  "Usages in a file of source-ns things other than the moved var, which still
  need the source require.

  Any var used through an alias, a refer or fully qualified needs it.
  Var-usages flagged `:refer` are the symbols inside the ns `:refer [...]`
  vector, not usages in code, so they are excluded.

  Keywords like `::a/foo` need the alias as well. `#::a{:foo 1}` does too, but
  its keywords can't be told apart from the ones of `#:source-ns{:foo 1}`, so
  both are kept."
  [local-buckets source-ns def-to-move]
  (concat
    (filter #(and (not (:refer %))
                  (= (:to %) source-ns)
                  (not= (:name %) (:name def-to-move)))
            (:var-usages local-buckets))
    (filter #(and (= (:ns %) source-ns)
                  (or (:alias %) (:namespace-from-prefix %)))
            (concat (:keyword-usages local-buckets)
                    (:keyword-definitions local-buckets)))))

(defn ^:private circular-dependency?
  "Whether the move would make source and destination require each other: the
  source would require the destination, as it still uses the moved var, while
  the destination keeps requiring the source, as it still uses other source-ns
  things, or doesn't use the moved var, in which case its ns isn't touched."
  [db def-to-move source-ns source-uri dest-uri dest-refs per-file-usages]
  (let [dest-buckets (get-in db [:analysis dest-uri])]
    (boolean
      (and (contains? per-file-usages source-uri)
           (some #(= (:name %) source-ns) (:namespace-usages dest-buckets))
           (or (empty? dest-refs)
               (seq (find-other-source-usages dest-buckets source-ns def-to-move)))))))

(defn ^:private determine-ns-edits
  "Edits to the ns form of a file that used the moved var: adds `libspec`, when
  given, and removes the source require, or only the `source-refer`, when no
  longer needed."
  [local-buckets file-loc def-to-move source-ns source-refer libspec uri db]
  (let [other-source-refers (filter #(and (:refer %)
                                          (= (:to %) source-ns)
                                          (not= (:name %) (:name def-to-move)))
                                    (:var-usages local-buckets))
        other-source-usages (find-other-source-usages local-buckets source-ns def-to-move)
        source-require (first (filter #(= (:name %) source-ns)
                                      (:namespace-usages local-buckets)))
        remove-source-require? (and source-require (empty? other-source-usages))
        namespace-loc (edit/find-namespace file-loc)]
    (if-let [add-to-ns-changes (when libspec
                                 (f.add-missing-libspec/add-to-namespace* file-loc uri libspec db))]
      (cond-> add-to-ns-changes
        remove-source-require?
        (update-in
          [0 :loc]
          (fn [loc]
            (z/subedit->
              loc
              (edit/find-at-element source-require)
              z/up
              z/remove)))

        (and (not remove-source-require?) source-refer)
        (update-in
          [0 :loc]
          (fn [loc]
            (z/subedit->
              loc
              (edit/find-at-element-name source-refer)
              z/remove
              (cond-> (empty? other-source-refers) (-> z/remove z/remove)))))

        :always
        (->> (f.add-missing-libspec/cleaning-ns-edits uri db)))
      (when (or remove-source-require? source-refer)
        (->> [{:loc (cond-> namespace-loc
                      remove-source-require?
                      (z/subedit->
                        (edit/find-at-element source-require)
                        z/up
                        z/remove)

                      (and (not remove-source-require?) source-refer)
                      (z/subedit->
                        (edit/find-at-element-name source-refer)
                        z/remove
                        (cond-> (empty? other-source-refers) (-> z/remove z/remove))))
               :range (meta (z/node namespace-loc))}]
             (f.add-missing-libspec/cleaning-ns-edits uri db))))))

(defn move-form [zloc source-uri {:keys [db*] :as components} dest-filename]
  (let [db @db*
        source-nses (vec (dep-graph/ns-names-for-uri db source-uri))
        dest-uri (-> dest-filename (shared/absolute-path db) (shared/filename->uri db))
        dest-nses (vec (dep-graph/ns-names-for-uri db dest-uri))
        unique-usage (juxt :uri :name :row :col)]
    (when (and (= 1 (count source-nses))
               (= 1 (count dest-nses)))
      (let [source-ns (first source-nses)
            dest-ns (first dest-nses)
            inner-usages (var-usages-within zloc source-uri db)
            ;; if source-ns things are used within the form
            ;; we can't move it
            local-inner-usages (->> inner-usages
                                    (filterv (comp #(= source-ns %) :to)))
            defs (var-definitions-within zloc source-uri db)
            form-loc (edit/to-top zloc)
            on-top-level-form? (= form-loc zloc)
            ;; You could have multiple in a top form (let [x 1] (def y x) (def z y))
            ;; we don't support that
            single-def? (= 1 (count defs))
            can-move? (and dest-ns
                           (empty? local-inner-usages)
                           on-top-level-form?
                           single-def?)]
        (when can-move?
          (let [def-to-move (first defs)
                refs (q/find-references db def-to-move false)
                dest-refs (filter (comp #(= % dest-uri) :uri) refs)
                per-file-usages (group-by :uri refs)]
            (if (circular-dependency? db def-to-move source-ns source-uri dest-uri dest-refs per-file-usages)
              {:error {:message (format "Can't move %s to %s: %s still uses it and %s requires %s, which would create a circular dependency."
                                        (:name def-to-move) dest-ns source-ns dest-ns source-ns)
                       :code :invalid-request}}
              (let [dest-loc (some-> (f.file-management/force-get-document-text dest-uri components)
                                     parser/z-of-string*)
                    insertion-loc (some-> dest-loc z/down z/rightmost)
                    insertion-pos (meta (z/node insertion-loc))
                    dest-inner-usages (->> inner-usages
                                           (filterv (comp #(= dest-ns %) :to))
                                           (medley/distinct-by unique-usage))
                    form-node (z/node (z/subedit-> form-loc (drop-ns-from-dest dest-inner-usages)))
                    ;; The refers live in the ns form, handled by the ns edits
                    dest-usages (->> dest-refs
                                     (remove :refer)
                                     (sort-by (juxt :name-row :name-col)))
                    ;; The destination may have used the moved var through the
                    ;; source ns, which it may not need to require anymore
                    dest-ns-changes (when (seq dest-refs)
                                      (let [db @db*
                                            local-buckets (get-in db [:analysis dest-uri])
                                            source-refer (find-source-refer local-buckets source-ns def-to-move)]
                                        (determine-ns-edits local-buckets dest-loc def-to-move source-ns source-refer nil dest-uri db)))
                    first-dest-usage-top-loc (some->> (first dest-usages)
                                                      (edit/find-at-element-name insertion-loc)
                                                      edit/to-top)
                    form-changes (if first-dest-usage-top-loc
                                   ;; Before its first usage, so it's defined when used
                                   [(r.transform/prepend-preserving-comment first-dest-usage-top-loc (z/of-node form-node))]
                                   (let [end-pos (assoc insertion-pos :row (:end-row insertion-pos) :col (:end-col insertion-pos))]
                                     [{:loc (z/of-string "\n\n")
                                       :range end-pos}
                                      {:loc (some-> insertion-loc (z/insert-left form-node) z/left)
                                       :range end-pos}]))
                    dest-changes (-> (vec dest-ns-changes)
                                     (into form-changes)
                                     (into (for [dest-usage dest-usages
                                                 :let [dest-usage-loc (edit/find-at-element-name insertion-loc dest-usage)]]
                                             {:loc (z/replace dest-usage-loc (symbol (name (z/sexpr dest-usage-loc))))
                                              :range (meta (z/node dest-usage-loc))})))
                    usage-changes-by-uri (->> (dissoc per-file-usages dest-uri)
                                              (medley/map-kv-vals
                                                (fn [file-uri usages]
                                                  (let [usage (first usages)
                                                        usage-uri (:uri usage)
                                                        file-loc (some-> (f.file-management/force-get-document-text file-uri components)
                                                                         parser/z-of-string*)
                                                        db @db*
                                                        local-buckets (get-in db [:analysis usage-uri])
                                                        source-refer (find-source-refer local-buckets source-ns def-to-move)

                                                        dest-require (first (filter #(= (:name %) dest-ns)
                                                                                    (:namespace-usages local-buckets)))
                                                        namespace-suggestions (f.add-missing-libspec/find-namespace-suggestions
                                                                                (str dest-ns)
                                                                                (f.add-missing-libspec/find-alias-ns-pairs db source-uri))
                                                        suggestion (if dest-require
                                                                     {:alias (str (:alias dest-require))}
                                                                     (first namespace-suggestions))
                                                        usages (->> (:var-usages local-buckets)
                                                                    (filter #(and (not (:refer %))
                                                                                  (= (:to %) source-ns)
                                                                                  (= (:name %) (:name def-to-move))))
                                                                    (medley/distinct-by unique-usage))
                                                        libspec (merge
                                                                  {:type :require
                                                                   :lib dest-ns}
                                                                  (when suggestion
                                                                    {:alias (some-> suggestion :alias symbol)})
                                                                  (when source-refer
                                                                    {:refer (:name source-refer)}))

                                                        ns-changes (determine-ns-edits local-buckets file-loc def-to-move source-ns source-refer libspec source-uri db)
                                                        replacement-ns (cond
                                                                         (:alias libspec)
                                                                         (:alias libspec)

                                                                         :else
                                                                         (:lib libspec))
                                                        usage-changes (keep (fn [usage]
                                                                              (let [usage-loc (edit/find-at-element-name file-loc usage)]
                                                                                (when (or
                                                                                        (= source-uri usage-uri)
                                                                                        (namespace (z/sexpr usage-loc)))
                                                                                  {:loc (z/replace usage-loc (symbol (str replacement-ns)
                                                                                                                     (str (:name def-to-move))))
                                                                                   :range (meta (z/node usage-loc))})))
                                                                            usages)]
                                                    (vec (concat ns-changes usage-changes)))))
                                              not-empty)
                    changes-by-uri (-> {dest-uri dest-changes}
                                       ;; Adjust requires and usages
                                       (merge usage-changes-by-uri)
                                       ;; Remove moved form
                                       (update source-uri (fnil conj []) {:loc nil
                                                                          :range (edit/range-with-left-whitespace zloc)}))]
                {:changes-by-uri changes-by-uri}))))))))

(ns clojure-lsp.feature.move-form
  (:require
   [clojure-lsp.feature.add-missing-libspec :as f.add-missing-libspec]
   [clojure-lsp.feature.file-management :as f.file-management]
   [clojure-lsp.queries :as q]
   [clojure-lsp.refactor.edit :as edit]
   [clojure-lsp.shared :as shared]
   [medley.core :as medley]
   [rewrite-clj.zip :as z]))

(defn drop-ns-from-dest [zloc usages]
  (loop [[usage & other-usages] usages
         loc zloc]
    (if usage
      (let [usage-loc (edit/find-at-usage-name loc usage)]
        (recur other-usages (z/subedit-> loc
                                         (edit/find-at-usage-name usage)
                                         (z/replace (symbol (name (z/sexpr usage-loc)))))))
      loc)))

(defn var-usages-within [zloc uri db]
  (let [{:keys [col row end-row end-col]} (meta (z/node zloc))
        analysis (:analysis db)
        defs (q/find-var-usages-under-form
               analysis
               (shared/uri->filename uri)
               row
               col
               end-row
               end-col)]
    (filterv
      #(edit/loc-encapsulates-usage? zloc %)
      defs)))

(defn var-definitions-within [zloc uri db]
  (let [analysis (:analysis db)
        defs (q/find-var-definitions
               analysis
               (shared/uri->filename uri)
               false)]
    (filterv
      #(edit/loc-encapsulates-usage? zloc %)
      defs)))

(defn ^:private determine-ns-edits [local-analysis file-loc def-to-move source-ns source-refer libspec uri db]
  (let [other-source-refers (filter #(and (:refer %)
                                          (= (:to %) source-ns)
                                          (not= (:name %) (:name def-to-move)))
                                    local-analysis)

        other-source-usages (filter #(and (not (:refer %))
                                          (not (:alias %))
                                          (= (:to %) source-ns)
                                          (not= (:name %) (:name def-to-move)))
                                    local-analysis)
        source-require (first (filter #(and (= :namespace-usages (:bucket %))
                                            (= (:name %) source-ns))
                                      local-analysis))
        remove-source-require? (and source-require (empty? other-source-usages))
        namespace-loc (edit/find-namespace file-loc)]
    (if-let [add-to-ns-changes (f.add-missing-libspec/add-to-namespace* file-loc libspec db)]
      (cond-> add-to-ns-changes
        remove-source-require?
        (update-in
          [0 :loc]
          (fn [loc]
            (z/subedit->
              loc
              (edit/find-at-usage source-require)
              z/up
              z/remove)))

        (and (not remove-source-require?) source-refer)
        (update-in
          [0 :loc]
          (fn [loc]
            (z/subedit->
              loc
              (edit/find-at-usage-name source-refer)
              z/remove
              (cond-> (empty? other-source-refers) (-> z/remove z/remove)))))

        :always
        (->> (f.add-missing-libspec/cleaning-ns-edits uri db)))
      (when (or remove-source-require? source-refer)
        (->> [{:loc (cond-> namespace-loc
                      remove-source-require?
                      (z/subedit->
                        (edit/find-at-usage source-require)
                        z/up
                        z/remove)

                      (and (not remove-source-require?) source-refer)
                      (z/subedit->
                        (edit/find-at-usage-name source-refer)
                        z/remove
                        (cond-> (empty? other-source-refers) (-> z/remove z/remove))))
               :range (meta (z/node namespace-loc))}]
             (f.add-missing-libspec/cleaning-ns-edits uri db))))))

(defn move-form [zloc uri db* dest-filename]
  (let [db @db*
        form-loc (edit/to-top zloc)
        analysis (:analysis db)
        source-filename (shared/uri->filename uri)
        source-ns (:name (q/find-namespace-definition-by-filename analysis source-filename db))
        dest-filename (shared/absolute-path dest-filename db)
        dest-ns (:name (q/find-namespace-definition-by-filename analysis dest-filename db))
        inner-usages (var-usages-within zloc uri db)
        ;; if source-ns things are used within the form
        ;; we can't move it
        local-inner-usages (->> inner-usages
                                (filterv (comp #(= source-ns %) :to)))
        defs (var-definitions-within zloc uri db)
        on-top-level-form? (= form-loc zloc)
        ;; You could have multiple in a top form (let [x 1] (def y x) (def z y))
        ;; we don't support that
        multiple-defs? (= 1 (count defs))
        can-move? (and dest-ns
                       (empty? local-inner-usages)
                       on-top-level-form?
                       multiple-defs?)]
    (when can-move?
      (let [def-to-move (first defs)
            refs (q/find-references analysis def-to-move false db)
            dest-refs (filter (comp #(= % dest-filename) :filename) refs)
            per-file-usages (group-by (comp #(shared/filename->uri % db) :filename) refs)
            dest-uri (shared/filename->uri dest-filename db)
            insertion-loc (some-> (f.file-management/force-get-document-text dest-uri db*)
                                  z/of-string
                                  z/rightmost)
            _db @db*
            insertion-pos (meta (z/node insertion-loc))
            dest-inner-usages (->> inner-usages
                                   (filterv (comp #(= dest-ns %) :to)))
            dest-changes (-> [{:loc (z/of-string "\n\n")
                               :range (assoc insertion-pos :row (:end-row insertion-pos) :col (:end-col insertion-pos))}
                              {:loc (some-> insertion-loc
                                            (z/insert-left (z/node (-> form-loc
                                                                       (z/subedit-> (drop-ns-from-dest dest-inner-usages)))))
                                            z/left)
                               :range (assoc insertion-pos :row (:end-row insertion-pos) :col (:end-col insertion-pos))}]
                             (into (for [dest-ref (vec dest-refs)
                                         :let [dest-ref-loc (edit/find-at-usage-name insertion-loc dest-ref)]]
                                     {:loc (z/replace dest-ref-loc (symbol (name (z/sexpr dest-ref-loc))))
                                      :range (meta dest-ref-loc)})))
            usage-changes-by-uri (->> (dissoc per-file-usages dest-uri)
                                      (medley/map-kv-vals
                                        (fn [file-uri usages]
                                          (let [usage (first usages)
                                                filename (:filename usage)
                                                file-loc (-> (f.file-management/force-get-document-text file-uri db*)
                                                             z/of-string)
                                                db @db*
                                                local-analysis (vec (get analysis filename))
                                                source-refer (first (filter #(and (:refer %)
                                                                                  (= (:to %) source-ns)
                                                                                  (= (:name %) (:name def-to-move)))
                                                                            local-analysis))

                                                dest-require (first (filter #(and (= :namespace-usages (:bucket %))
                                                                                  (= (:name %) dest-ns))
                                                                            local-analysis))
                                                namespace-suggestions (f.add-missing-libspec/find-namespace-suggestions
                                                                        (str dest-ns)
                                                                        (f.add-missing-libspec/find-alias-ns-pairs analysis uri db))
                                                suggestion (if dest-require
                                                             {:alias (str (:alias dest-require))}
                                                             (first namespace-suggestions))
                                                usages (filter #(and (not (:refer %))
                                                                     (= (:to %) source-ns)
                                                                     (= (:name %) (:name def-to-move)))
                                                               local-analysis)
                                                libspec (merge
                                                          {:type :require
                                                           :lib dest-ns}
                                                          (when suggestion
                                                            {:alias (some-> suggestion :alias symbol)})
                                                          (when source-refer
                                                            {:refer (:name source-refer)}))

                                                ns-changes (determine-ns-edits local-analysis file-loc def-to-move source-ns source-refer libspec uri db)
                                                replacement-ns (cond
                                                                 (:alias libspec)
                                                                 (:alias libspec)

                                                                 :else
                                                                 (:lib libspec))
                                                usage-changes (keep (fn [usage]
                                                                      (let [usage-loc (edit/find-at-usage-name file-loc usage)]
                                                                        (when (or
                                                                                (= source-filename filename)
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
                               (update uri (fnil conj []) {:loc nil
                                                           :range (edit/range-with-left-whitespace zloc)}))]
        {:changes-by-uri changes-by-uri}))))

(ns clojure-lsp.feature.move-form
  (:require
    [clojure-lsp.refactor.edit :as edit]
    [rewrite-clj.zip :as z]
    [clojure-lsp.queries :as q]
    [clojure-lsp.shared :as shared]
    [medley.core :as medley]
    [clojure-lsp.parser :as parser]
    [clojure-lsp.feature.add-missing-libspec :as f.add-missing-libspec]))

(defn drop-ns-from-dest [zloc usages]
  (loop [[usage & other-usages] usages
         loc zloc]
    (if usage
      (let [usage-loc (edit/find-at-pos loc (:name-row usage) (:name-col usage))]
        (recur other-usages (z/subedit-> loc
                                         (edit/find-at-pos (:name-row usage) (:name-col usage))
                                         (z/replace (symbol (name (z/sexpr usage-loc)))))))
      loc)))

(defn var-usages-within [zloc uri db]
  (let [{:keys [col row end-row end-col]} (meta (z/node zloc))
        analysis (:analysis @db)
        defs (q/find-var-usages-under-form
               analysis
               (shared/uri->filename uri)
               row
               col
               end-row
               end-col)]
    (->> defs
         (filterv #(edit/in-range? (update (meta (z/node zloc)) :end-col inc) %)))))

(defn var-definitions-within [zloc uri db]
  (let [analysis (:analysis @db)
        defs (q/find-var-definitions
               analysis
               (shared/uri->filename uri)
               false)]
    (filterv
      #(edit/in-range? (update (meta (z/node zloc)) :end-col inc) %)
      defs)))

(defn move-form [zloc uri db dest-filename]
  (let [form-loc (edit/to-top zloc)
        analysis (:analysis @db)
        source-filename (shared/uri->filename uri)
        source-ns (:name (q/find-namespace-definition-by-filename analysis source-filename db))
        dest-filename (shared/absolute-path dest-filename db)
        dest-ns (:name (q/find-namespace-definition-by-filename analysis dest-filename db))
        inner-usages (var-usages-within zloc uri db)
        ;; if source-ns things are used within the form
        ;; we can't move it
        local-inner-usages (->> inner-usages
                                (filterv (comp #(= source-ns %) :to)))
        ;; You could have multiple in a top form (let [x 1] (def y x) (def z y))
        ;; we don't support that
        defs (var-definitions-within zloc uri db)]
    (when (and dest-ns (empty? local-inner-usages) (= form-loc zloc) (= 1 (count defs)))
      (let [def-to-move (first defs)
            refs (q/find-references analysis def-to-move false true)
            dest-refs (filter (comp #(= % dest-filename) :filename) refs)
            per-ns-info (->> refs
                             (group-by :from)
                             (medley/map-vals
                               (fn [usages]
                                 (let [usage (first usages)
                                       filename (:filename usage)
                                       file-uri (shared/filename->uri filename db)
                                       file-loc (parser/safe-zloc-of-file @db file-uri)
                                       namespace-suggestions (f.add-missing-libspec/find-namespace-suggestions
                                                               (str dest-ns)
                                                               (f.add-missing-libspec/find-alias-ns-pairs analysis uri db))]
                                   {:suggestion (first namespace-suggestions)
                                    :file-loc file-loc
                                    :filename filename
                                    :file-uri file-uri}))))

            dest-uri (shared/filename->uri dest-filename db)
            insertion-loc (some-> (parser/safe-zloc-of-file @db dest-uri)
                                  z/rightmost)
            insertion-pos (meta (z/node insertion-loc))
            dest-inner-usages (->> inner-usages
                                   (filterv (comp #(= dest-ns %) :to)))
            dest-changes (-> [{:loc (z/of-string "\n")
                               :range (assoc insertion-pos :row (:end-row insertion-pos) :col (:end-col insertion-pos))}
                              {:loc (some-> insertion-loc
                                            (z/insert-left (z/node (-> form-loc
                                                                       (z/subedit-> (drop-ns-from-dest dest-inner-usages)))))
                                            z/left)
                               :range (assoc insertion-pos :row (:end-row insertion-pos) :col (:end-col insertion-pos))}]
                             (into (for [dest-ref (vec dest-refs)
                                         :let [dest-ref-loc (some-> insertion-loc
                                                                    (z/leftmost)
                                                                    (edit/find-at-pos (:name-row dest-ref) (:name-col dest-ref)))]]
                                     {:loc (z/replace dest-ref-loc (symbol (name (z/sexpr dest-ref-loc))))
                                      :range (meta dest-ref-loc)})))
            usage-changes-by-uri (->> (dissoc per-ns-info dest-ns)
                                      (medley/map-kv
                                        (fn [_current-ns {:keys [suggestion file-loc file-uri filename]}]
                                          (let [local-analysis (vec (get analysis filename))
                                                source-refer (first (filter #(and (:refer %)
                                                                                  (= (:to %) source-ns)
                                                                                  (= (:name %) (:name def-to-move)))
                                                                            local-analysis))

                                                other-source-refers (filter #(and (:refer %)
                                                                                  (= (:to %) source-ns)
                                                                                  (not= (:name %) (:name def-to-move)))
                                                                            local-analysis)
                                                source-require (first (filter #(and (= :namespace-usages (:bucket %))
                                                                                    (= (:name %) source-ns))
                                                                              local-analysis))
                                                usages (filter #(and (not (:refer %))
                                                                     (= (:to %) source-ns)
                                                                     (= (:name %) (:name def-to-move)))
                                                               local-analysis)
                                                other-source-usages (filter #(and (not (:refer %))
                                                                                  (not (:alias %))
                                                                                  (= (:to %) source-ns)
                                                                                  (not= (:name %) (:name def-to-move)))
                                                                            local-analysis)
                                                libspec (merge
                                                          {:type :require
                                                           :lib dest-ns}
                                                          (when suggestion
                                                            {:alias (some-> suggestion :alias symbol)})
                                                          (when source-refer
                                                            {:refer (:name source-refer)}))
                                                remove-source-require? (and source-require (empty? other-source-usages))
                                                ns-changes (cond-> (f.add-missing-libspec/add-to-namespace* file-loc libspec db)
                                                             remove-source-require?
                                                             (update-in
                                                               [0 :loc]
                                                               (fn [loc]
                                                                 (z/subedit->
                                                                   loc
                                                                   (edit/find-at-pos (:row source-require)
                                                                                     (:col source-require))
                                                                   z/up
                                                                   z/remove)))

                                                             (and (not remove-source-require?) source-refer)
                                                             (update-in
                                                               [0 :loc]
                                                               (fn [loc]
                                                                 (z/subedit->
                                                                   loc
                                                                   (edit/find-at-pos (:name-row source-refer)
                                                                                     (:name-col source-refer))
                                                                   z/remove
                                                                   (cond-> (empty? other-source-refers) (-> z/remove z/remove))))))
                                                replacement-ns (cond
                                                                 (:alias libspec)
                                                                 (:alias libspec)

                                                                 :else
                                                                 (:lib libspec))
                                                usage-changes (keep (fn [usage]
                                                                      (let [usage-loc (edit/find-at-pos file-loc
                                                                                                        (:name-row usage)
                                                                                                        (:name-col usage))]
                                                                        (when (or
                                                                                (= source-filename filename)
                                                                                (namespace (z/sexpr usage-loc)))
                                                                          {:loc (z/replace usage-loc (symbol (str replacement-ns)
                                                                                                             (str (:name def-to-move))))
                                                                           :range (meta (z/node usage-loc))})))
                                                                    usages)]
                                            [file-uri (vec (concat ns-changes usage-changes))])))

                                      not-empty)

            changes-by-uri (-> {dest-uri dest-changes}
                               (merge usage-changes-by-uri)
                               (update uri (fnil conj []) {:loc nil :range (meta (z/node zloc))}))]
        {:changes-by-uri changes-by-uri}))))

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
   [clojure.string :as string]
   [medley.core :as medley]
   [rewrite-clj.node :as n]
   [rewrite-clj.zip :as z]))

(def ^:private core-nses '#{clojure.core cljs.core})

(defn ^:private written-symbol
  "The symbol written in the code for the `usage`, unless it doesn't come
  from one, like the usages derived from macros."
  [zloc usage]
  (let [sym (some-> (edit/find-at-element-name zloc usage)
                    f.add-missing-libspec/safe-sexpr)]
    (when (and (symbol? sym)
               (= (name sym) (str (:name usage))))
      sym)))

(defn ^:private replace-usages
  "Replaces the symbols of the usages in `zloc`, given as `[usage new-sym]`."
  [zloc replacements]
  (reduce (fn [loc [usage new-sym]]
            (z/subedit-> loc
                         (edit/find-at-element-name usage)
                         (z/replace new-sym)))
          zloc
          ;; a replaced node loses its position, so it can't be found again
          (medley/distinct-by (comp (juxt :name-row :name-col) first) replacements)))

(defn ^:private rewrite-auto-resolved-keywords
  "Keeps the meaning of the auto-resolved keywords and namespaced maps of the
  moved form in the destination: `::foo` and `#::{}`, of the source ns, are
  qualified with it, while `::a/foo` and `#::a{}` use the destination alias
  for the lib, if any, are qualified with it otherwise, or drop the alias
  when the lib is the destination ns."
  [zloc {:keys [source-ns dest-ns source-alias->lib lib->dest-alias]}]
  (let [;; [auto-resolved? prefix] for the given source prefix, nil if unknown
        dest-qualifier (fn [prefix]
                         (let [lib (if prefix (get source-alias->lib (symbol prefix)) source-ns)
                               dest-alias (when (not= source-ns lib) (get lib->dest-alias lib))]
                           (cond
                             (nil? lib) nil
                             (= dest-ns lib) [true nil]
                             dest-alias [true (str dest-alias)]
                             :else [false (str lib)])))]
    (z/prewalk zloc
               #(:auto-resolved? (z/node %))
               (fn [loc]
                 (let [node (z/node loc)]
                   (if (n/keyword-node? node)
                     (if-let [[auto-resolved? prefix] (dest-qualifier (namespace (:k node)))]
                       (z/replace loc (n/keyword-node (keyword prefix (name (:k node))) auto-resolved?))
                       loc)
                     (if-let [[auto-resolved? prefix] (dest-qualifier (:prefix node))]
                       (z/replace loc (n/map-qualifier-node auto-resolved? prefix))
                       loc)))))))

(defn ^:private lib-requirements
  "What the moved form usages of `lib` need to keep working in the
  destination: the `:libspecs` to add to it and the `:replacements` of their
  symbols, as `[usage new-sym]`.

  It reuses the destination alias for the lib, or adds the source one when
  free, otherwise qualifies the usages with the lib. Refers are added when
  their name is free in the destination, otherwise qualified as well."
  [lib usages {:keys [dest-alias->lib dest-lib->alias dest-required-libs dest-referred dest-var-names lang]}]
  (let [required? (contains? dest-required-libs lib)
        alias-usages (filter :alias usages)
        refer-usages (remove (comp namespace :sym) usages)
        ;; analysis symbols carry their position, which would be printed
        without-meta #(cond-> % (instance? clojure.lang.IObj %) (with-meta nil))
        lib (without-meta lib)
        source-alias (some-> (some :alias alias-usages) without-meta)
        alias-to-add (when (and source-alias
                                (not required?)
                                (not (contains? dest-alias->lib source-alias)))
                       source-alias)
        dest-alias (or (get dest-lib->alias lib) alias-to-add)
        qualify (fn [usage] [usage (symbol (str (or dest-alias lib)) (str (:name usage)))])
        referred? #(= lib (get dest-referred (:name %)))
        free? #(not (or (contains? dest-referred (:name %))
                        (contains? dest-var-names (:name %))))
        refers-to-add (->> refer-usages (remove referred?) (filter free?) (map (comp without-meta :name)) distinct)]
    {:alias dest-alias
     :new-lib? (not required?)
     :libspecs (cond-> []
                 (not required?) (conj (cond-> {:type :require :lib lib}
                                         alias-to-add (assoc :alias alias-to-add)
                                         lang (assoc :lang lang)))
                 :always (into (map #(hash-map :type :require :lib lib :refer %)) refers-to-add))
     :replacements (concat (->> alias-usages
                                (remove #(= dest-alias (:alias %)))
                                (map qualify))
                           (->> refer-usages
                                (remove referred?)
                                (remove free?)
                                (map qualify)))}))

(defn ^:private single-lang
  "The lang of the `usages` when it's the only one but the destination has
  others, as in cljc files, so what they need must be within a reader
  conditional."
  [usages dest-langs]
  (let [langs (into #{} (keep :lang) usages)]
    (when (and (= 1 (count langs))
               (< 1 (count dest-langs)))
      (first langs))))

(defn ^:private import-requirements
  "What the classes the moved form imports from the source need in the
  destination: the `:libspecs` of the imports to add to it, and the
  `:replacements` fully qualifying the classes whose name is taken by another
  class imported in the destination."
  [form-loc source-buckets dest-buckets dest-langs]
  (let [imported (fn [buckets] (into #{} (comp (filter :import) (map :class)) (:java-class-usages buckets)))
        simple-name #(last (string/split % #"\."))
        source-imports (imported source-buckets)
        dest-imports (imported dest-buckets)
        dest-simple-names (into #{} (map simple-name) dest-imports)
        ;; e.g. `Date`, `Date.` or `Date/from`, but not `java.util.Date.`
        written-class (fn [{:keys [class] :as usage}]
                        (let [sym (some-> (edit/find-at-element-name form-loc usage)
                                          f.add-missing-libspec/safe-sexpr)]
                          (when (and (symbol? sym)
                                     (= (simple-name class)
                                        (or (namespace sym)
                                            (string/replace (name sym) #"\.$" ""))))
                            sym)))
        all-usages (->> (:java-class-usages source-buckets)
                        (remove :import)
                        (filter #(contains? source-imports (:class %)))
                        (filter #(edit/loc-encapsulates-usage? form-loc %))
                        (keep #(some->> (written-class %) (assoc % :sym))))
        class->lang (->> all-usages
                         (group-by :class)
                         (medley/map-vals #(single-lang % dest-langs)))
        {conflicting true missing false} (->> all-usages
                                              (medley/distinct-by (juxt :name-row :name-col))
                                              (remove #(contains? dest-imports (:class %)))
                                              (group-by #(contains? dest-simple-names (simple-name (:class %)))))]
    {:libspecs (->> missing
                    (map :class)
                    distinct
                    (map (fn [class]
                           (cond-> {:type :import
                                    :lib (symbol (string/replace class #"\.[^.]+$" ""))
                                    :class (symbol (simple-name class))}
                             (class->lang class) (assoc :lang (class->lang class))))))
     :replacements (for [{:keys [class sym] :as usage} conflicting]
                     [usage (cond
                              (namespace sym) (symbol class (name sym))
                              (string/ends-with? (name sym) ".") (symbol (str class "."))
                              :else (symbol class))])}))

(defn ^:private form-requirements
  "What the moved form needs to keep working in the destination, given the
  usages of the form of libs other than the source and destination ones: the
  `:libspecs` to add to the destination, the `:new-libs` it'll require, the
  `:replacements` of the symbols of the form, as `[usage new-sym]`, and the
  aliases the destination will have for the libs, in `:lib->alias`."
  [{:keys [form-loc form-usages source-buckets dest-buckets dest-langs]}]
  (let [dest-requires (:namespace-usages dest-buckets)
        ctx {:dest-alias->lib (into {} (keep #(when (:alias %) [(:alias %) (:name %)])) dest-requires)
             :dest-lib->alias (into {} (keep #(when (:alias %) [(:name %) (:alias %)])) dest-requires)
             :dest-required-libs (into #{} (map :name) dest-requires)
             :dest-referred (into {} (comp (filter :refer) (map (juxt :name :to))) (:var-usages dest-buckets))
             :dest-var-names (into #{} (map :name) (:var-definitions dest-buckets))}
        usages (keep #(some->> (written-symbol form-loc %) (assoc % :sym)) form-usages)
        lib->lang (->> usages
                       (group-by :to)
                       (medley/map-vals #(single-lang % dest-langs)))
        lib->requirements (->> usages
                               (medley/distinct-by (juxt :to :name-row :name-col))
                               (group-by :to)
                               (medley/map-kv-vals #(lib-requirements %1 %2 (assoc ctx :lang (lib->lang %1)))))
        imports (import-requirements form-loc source-buckets dest-buckets dest-langs)]
    {:lib->alias (into (:dest-lib->alias ctx)
                       (keep (fn [[lib {:keys [alias]}]] (when alias [lib alias])))
                       lib->requirements)
     :new-libs (keep (fn [[lib {:keys [new-lib?]}]] (when new-lib? lib)) lib->requirements)
     :libspecs (concat (mapcat :libspecs (vals lib->requirements))
                       (:libspecs imports))
     :replacements (concat (mapcat :replacements (vals lib->requirements))
                           (:replacements imports))}))

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

(defn ^:private wrap-in-reader-conditional
  "Wraps the libspec of `lib` in a `#?(lang ...)` reader conditional, unless
  it's already in one."
  [ns-loc lib lang]
  (z/subedit-> ns-loc
               (as-> loc
                     (if-let [libspec-loc (some-> (z/find-value loc z/next lib) z/up)]
                       (if (= :reader-macro (some-> libspec-loc z/up z/up z/tag))
                         libspec-loc
                         (z/replace libspec-loc (n/reader-macro-node
                                                  [(n/token-node '?)
                                                   (n/list-node [(n/keyword-node lang)
                                                                 (n/spaces 1)
                                                                 (z/node libspec-loc)])])))
                       loc))))

(defn ^:private with-libspecs
  "The ns loc with the `libspecs` added, keeping the position metadata of its
  existing nodes. The libspecs with a `:lang` are added within a reader
  conditional for it."
  [ns-loc libspecs uri db]
  (reduce (fn [ns-loc {:keys [lib lang] :as libspec}]
            (if-let [[{:keys [loc]}] (f.add-missing-libspec/add-to-namespace* ns-loc uri libspec db)]
              (cond-> (z/replace ns-loc (z/root loc))
                lang (wrap-in-reader-conditional lib lang))
              ns-loc))
          ns-loc
          libspecs))

(defn ^:private without-unneeded-source-require
  "The ns loc without the source require, or only the `source-refer`, when the
  file doesn't need them after the move."
  [ns-loc {:keys [local-buckets def-to-move source-ns source-refer]}]
  (let [other-source-refers (filter #(and (:refer %)
                                          (= (:to %) source-ns)
                                          (not= (:name %) (:name def-to-move)))
                                    (:var-usages local-buckets))
        source-require (first (filter #(= (:name %) source-ns)
                                      (:namespace-usages local-buckets)))]
    (cond
      (and source-require
           (empty? (find-other-source-usages local-buckets source-ns def-to-move)))
      (z/subedit-> ns-loc
                   (edit/find-at-element source-require)
                   z/up
                   z/remove)

      source-refer
      (z/subedit-> ns-loc
                   (edit/find-at-element-name source-refer)
                   z/remove
                   (cond-> (empty? other-source-refers) (-> z/remove z/remove)))

      :else
      ns-loc)))

(defn ^:private determine-ns-edits
  "Edits to the ns form of a file: adds the `libspecs` and, when the file used
  the moved var, removes the source require, or only the `source-refer`, when
  no longer needed."
  [{:keys [file-loc libspecs used-moved-var? uri db] :as opts}]
  (if-let [ns-loc (edit/find-namespace file-loc)]
    (let [new-ns-loc (cond-> (with-libspecs ns-loc libspecs uri db)
                       used-moved-var? (without-unneeded-source-require opts))]
      (when-not (identical? (z/node ns-loc) (z/node new-ns-loc))
        (f.add-missing-libspec/cleaning-ns-edits
          uri db [{:loc new-ns-loc
                   :range (meta (z/node ns-loc))}])))
    ;; Adding a libspec to a file without ns form creates it
    (when-let [libspec (first libspecs)]
      (some->> (f.add-missing-libspec/add-to-namespace* file-loc uri libspec db)
               (f.add-missing-libspec/cleaning-ns-edits uri db)))))

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
                per-file-usages (group-by :uri refs)
                ;; looking usages up only within the form
                form-zip (z/subzip form-loc)
                form-reqs (form-requirements {:form-loc form-zip
                                              :form-usages (remove #(or (nil? (:to %))
                                                                        (contains? core-nses (:to %))
                                                                        (contains? #{source-ns dest-ns :clj-kondo/unknown-namespace} (:to %)))
                                                                   inner-usages)
                                              :source-buckets (get-in db [:analysis source-uri])
                                              :dest-buckets (get-in db [:analysis dest-uri])
                                              :dest-langs (shared/uri->available-langs dest-uri)})
                lib-requiring-dest (first (filter #(dep-graph/ns-depends-on? db % dest-ns) (:new-libs form-reqs)))]
            (cond
              (circular-dependency? db def-to-move source-ns source-uri dest-uri dest-refs per-file-usages)
              {:error {:message (format "Can't move %s to %s: %s still uses it and %s requires %s, which would create a circular dependency."
                                        (:name def-to-move) dest-ns source-ns dest-ns source-ns)
                       :code :invalid-request}}

              lib-requiring-dest
              {:error {:message (format "Can't move %s to %s: it uses %s, which requires %s, so it would create a circular dependency."
                                        (:name def-to-move) dest-ns lib-requiring-dest dest-ns)
                       :code :invalid-request}}

              :else
              (let [dest-loc (some-> (f.file-management/force-get-document-text dest-uri components)
                                     parser/z-of-string*)
                    insertion-loc (some-> dest-loc z/down z/rightmost)
                    insertion-pos (meta (z/node insertion-loc))
                    dest-inner-usages (->> inner-usages
                                           (filterv (comp #(= dest-ns %) :to))
                                           (medley/distinct-by unique-usage))
                    form-node (z/node (z/subedit-> form-loc
                                                   (replace-usages (concat (for [usage dest-inner-usages
                                                                                 :when (written-symbol form-zip usage)]
                                                                             [usage (symbol (str (:name usage)))])
                                                                           (:replacements form-reqs)))
                                                   (rewrite-auto-resolved-keywords
                                                     {:source-ns source-ns
                                                      :dest-ns dest-ns
                                                      :source-alias->lib (into {}
                                                                               (keep #(when (:alias %) [(:alias %) (:name %)]))
                                                                               (get-in db [:analysis source-uri :namespace-usages]))
                                                      :lib->dest-alias (:lib->alias form-reqs)})))
                    ;; The refers live in the ns form, handled by the ns edits
                    dest-usages (->> dest-refs
                                     (remove :refer)
                                     (sort-by (juxt :name-row :name-col)))
                    ;; Adds what the moved form needs and, when the destination
                    ;; used the moved var through the source ns, removes the
                    ;; source require it may not need anymore
                    dest-ns-changes (let [db @db*
                                          local-buckets (get-in db [:analysis dest-uri])]
                                      (determine-ns-edits {:file-loc dest-loc
                                                           :libspecs (:libspecs form-reqs)
                                                           :used-moved-var? (seq dest-refs)
                                                           :local-buckets local-buckets
                                                           :def-to-move def-to-move
                                                           :source-ns source-ns
                                                           :source-refer (find-source-refer local-buckets source-ns def-to-move)
                                                           :uri dest-uri
                                                           :db db}))
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

                                                        ns-changes (determine-ns-edits {:file-loc file-loc
                                                                                        :libspecs [libspec]
                                                                                        :used-moved-var? true
                                                                                        :local-buckets local-buckets
                                                                                        :def-to-move def-to-move
                                                                                        :source-ns source-ns
                                                                                        :source-refer source-refer
                                                                                        :uri usage-uri
                                                                                        :db db})
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

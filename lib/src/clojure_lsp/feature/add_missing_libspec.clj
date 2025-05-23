(ns clojure-lsp.feature.add-missing-libspec
  (:require
   [clojure-lsp.common-symbols :as common-sym]
   [clojure-lsp.dep-graph :as dep-graph]
   [clojure-lsp.feature.clean-ns :as f.clean-ns]
   [clojure-lsp.parser :as parser]
   [clojure-lsp.producer :as producer]
   [clojure-lsp.queries :as q]
   [clojure-lsp.refactor.edit :as edit]
   [clojure-lsp.settings :as settings]
   [clojure-lsp.shared :as shared :refer [fast=]]
   [clojure.string :as string]
   [medley.core :as medley]
   [rewrite-clj.node :as n]
   [rewrite-clj.paredit :as paredit]
   [rewrite-clj.zip :as z]
   [rewrite-clj.zip.subedit :as zsub]))

(set! *warn-on-reflection* true)

(defn safe-sexpr [zloc]
  (when (z/sexpr-able? zloc)
    ;; TODO There are cases where a invalid code {:foo b|} -> {:foo |}
    ;; causes a invalid sexpr but sexpr-able? returns true,
    ;; try to fix on the root cause
    (try
      (z/sexpr zloc)
      (catch Exception _))))

(defn safe-sym [zloc]
  (when-let [s (safe-sexpr zloc)]
    (cond
      (keyword? s)
      (when-let [kw-str (z/string zloc)]
        (when (string/starts-with? kw-str "::")
          (symbol (subs kw-str 2))))

      (symbol? s)
      s)))

(defn ^:private resolve-ns-inner-blocks-identation [db]
  (or (settings/get db [:clean :ns-inner-blocks-indentation])
      (if (settings/get db [:keep-require-at-start?])
        :same-line
        :next-line)))

(defn cleaning-ns-edits [uri db edits]
  (if (settings/get db [:clean :automatically-after-ns-refactor] true)
    (->> edits
         (map (fn [{:keys [loc range] :as edit}]
                (if (z/find-value loc z/next 'ns)
                  ;; re-read zloc to get a unchanged zloc with :forms
                  (some-> loc
                          z/root-string
                          parser/z-of-string*
                          (f.clean-ns/clean-ns-edits uri db)
                          first
                          (assoc :range range))
                  edit)))
         seq)
    edits))

(defn ^:private find-missing-ns-alias-require [zloc uri db]
  (let [require-alias (some-> zloc safe-sym namespace symbol)
        alias->info (->> (dep-graph/ns-aliases-for-langs db (shared/uri->available-langs uri))
                         (group-by :alias))
        possibilities (or (some->> (get alias->info require-alias)
                                   (medley/distinct-by (juxt :to))
                                   (map :to))
                          (->> [(get common-sym/common-alias->info require-alias)]
                               (remove nil?)))]
    (when (= 1 (count possibilities))
      {:ns (some-> possibilities first name symbol)
       :alias require-alias})))

(defn ^:private find-missing-ns-refer-require [zloc]
  (let [refer-to-add (safe-sym zloc)
        ns-loc (edit/find-namespace zloc)
        ns-zip (zsub/subzip ns-loc)]
    (when (not (z/find-value ns-zip z/next refer-to-add))
      (when-let [refer (get common-sym/common-refers->info refer-to-add)]
        {:ns refer
         :refer refer-to-add}))))

(defn find-missing-ns-require
  "Returns map with found ns and alias or refer."
  [zloc uri db]
  (if (some-> zloc safe-sym namespace)
    (find-missing-ns-alias-require zloc uri db)
    (find-missing-ns-refer-require zloc)))

(defn ^:private find-class-name [zloc]
  (when-let [sym (safe-sym zloc)]
    (let [value (z/string zloc)
          class? (and (first value)
                      (Character/isUpperCase ^Character (first value)))
          dot-call? (and (string/includes? value ".")
                         (= (dec (count value)) (.indexOf ^String value ".")))]
      (cond
        (not class?)
        nil

        (namespace sym)
        (some-> sym namespace (string/split #"\.") last)

        dot-call?
        (->> value drop-last (string/join ""))

        :else value))))

(defn find-missing-imports [zloc db]
  (when-let [class-name (find-class-name zloc)]
    (into []
          (comp
            (q/xf-all-java-definitions-by-class-name class-name)
            (map :class))
          (:analysis db))))

(defn ^:private find-when-starts-with-sym [zloc sym]
  (z/find-next zloc z/next #(and (identical? :token (z/tag %))
                                 (if (identical? :vector (z/tag (z/up %)))
                                   (= (z/string %) (str sym))
                                   (string/starts-with? (z/string %) (str sym))))))

(defn ^:private need-to-add-libspec?
  [zloc lib-sym refer-sym alias-sym class-sym]
  (or
                             ;; missing import
    (and class-sym (not (z/find-value zloc z/next class-sym)))
                             ;; missing namespace
    (and lib-sym (not (z/find-value zloc z/next lib-sym)))
                             ;; missing refer
    (and refer-sym (not (z/find-value zloc z/next refer-sym)))
                             ;; missing alias
    (and alias-sym (not (z/find-value zloc z/next alias-sym)))))

(defn ^:private modified-existing-libform
  [ns-zip form-type-loc lib-sym refer-sym _ class-sym]
  (let [existing-unwrapped-require (when refer-sym (z/find-value ns-zip z/next lib-sym))
        existing-wrapped-require (when existing-unwrapped-require
                                   (let [wrapped-require (z/up existing-unwrapped-require)]
                                     (when (fast= :vector (z/tag wrapped-require))
                                       wrapped-require)))
        existing-refer (when existing-wrapped-require
                         (z/find-value (zsub/subzip existing-wrapped-require) z/next ':refer))
        existing-import-package-only (when class-sym
                                       (z/find-value (z/up form-type-loc) z/next lib-sym))
        existing-import-package-by-segment (when (and existing-import-package-only
                                                      (#{:list :vector} (z/tag (z/prev existing-import-package-only))))
                                             (z/up existing-import-package-only))
        existing-import-package-by-full-package (when (and class-sym
                                                           (not existing-import-package-by-segment))
                                                  (find-when-starts-with-sym ns-zip lib-sym))
        existing-import-package-by-single-full-package (when (and existing-import-package-by-full-package
                                                                  (not (find-when-starts-with-sym existing-import-package-by-full-package lib-sym)))
                                                         existing-import-package-by-full-package)]
    {:existing-import-package-by-full-package
     existing-import-package-by-full-package
     :result-loc (cond
                   existing-refer
                   (z/subedit-> ns-zip
                                (z/find-value z/next lib-sym)
                                (z/find-value z/next ':refer)
                                z/right
                                (z/append-child* (n/spaces 1))
                                (z/append-child refer-sym))

                   existing-wrapped-require
                   (z/subedit-> ns-zip
                                (z/find-value z/next lib-sym)
                                z/up
                                (z/append-child* (n/spaces 1))
                                (z/append-child :refer)
                                (z/append-child [refer-sym]))

                   existing-unwrapped-require
                   (z/subedit-> ns-zip
                                (z/find-value z/next lib-sym)
                                (paredit/wrap-around :vector)
                                z/up
                                (z/append-child* (n/spaces 1))
                                (z/append-child :refer)
                                (z/append-child [refer-sym]))

                   existing-import-package-by-segment
                   (z/subedit-> ns-zip
                                (z/find-value z/next lib-sym)
                                z/up
                                (z/append-child* (n/spaces 1))
                                (z/append-child class-sym))

                   existing-import-package-by-single-full-package
                   (let [existing-class-name (symbol (last (string/split (z/string existing-import-package-by-single-full-package) #"\.")))]
                     (z/subedit-> ns-zip
                                  (z/find-next z/next #(= existing-import-package-by-single-full-package %))
                                  (z/replace [lib-sym existing-class-name class-sym])))

                   :else nil)}))

(defn add-to-namespace*
  [zloc
   {libspec-type :type
    lib-sym :lib
    refer-sym :refer
    alias-sym :alias
    class-sym :class}
   db]
  (let [ns-loc (edit/find-namespace zloc)
        ns-zip (zsub/subzip ns-loc)]
    (when (need-to-add-libspec? ns-zip lib-sym refer-sym alias-sym class-sym)
      (let [add-form-type? (not (z/find-value ns-zip z/next libspec-type))
            form-type-loc (z/find-value (zsub/subzip ns-loc) z/next libspec-type)
            ns-inner-blocks-indentation (resolve-ns-inner-blocks-identation db)
            col (if form-type-loc
                  (-> form-type-loc z/rightmost z/node meta :col)
                  (if (fast= :same-line ns-inner-blocks-indentation)
                    2
                    5))
            {:keys [result-loc existing-import-package-by-full-package]}
            (modified-existing-libform ns-zip form-type-loc lib-sym refer-sym alias-sym class-sym)
            form-to-add (cond
                          (and (fast= :import libspec-type)
                               existing-import-package-by-full-package)
                          (with-meta (symbol (str lib-sym "." class-sym)) nil)

                          (fast= :import libspec-type)
                          [(with-meta lib-sym nil)
                           (with-meta class-sym nil)]

                          (fast= :require libspec-type)
                          (cond-> (vector
                                    (if (string? lib-sym)
                                      lib-sym
                                      (with-meta lib-sym nil)))
                            alias-sym (conj :as (with-meta alias-sym nil))
                            refer-sym (conj :refer [(with-meta refer-sym nil)])))
            result-loc (or result-loc ;if not modified lib add new entry
                           (z/subedit-> ns-zip
                                        (cond->
                                         add-form-type? (z/append-child (n/newlines 1))
                                         add-form-type? (z/append-child (n/spaces 2))
                                         add-form-type? (z/append-child (list libspec-type)))
                                        (z/find-value z/next libspec-type)
                                        (z/up)
                                        (cond->
                                         (or (not add-form-type?)
                                             (fast= :next-line ns-inner-blocks-indentation)) (z/append-child* (n/newlines 1)))
                                        (z/append-child* (n/spaces (dec col)))
                                        (z/append-child form-to-add)))]
        [{:range (meta (z/node result-loc))
          :loc result-loc}]))))

(defn ^:private add-to-rcf?
  "returns zloc of rcf when require/import should be added to rcf-form, otherwise nil"
  [zloc mode db producer]
  (let [rcf-zloc (edit/inside-rcf? zloc)
        c-setting (settings/get db [:add-missing :add-to-rcf])]
    (when (and
            rcf-zloc
            (or
              (fast= :always c-setting)
              (and
                (not= :never c-setting)
                producer
                (= "Yes"
                   (producer/show-message-request
                     producer
                     (format "Add %s inside this comment form?" (name mode))
                     :info
                     [{:title "Yes"}
                      {:title "No"}]))))) rcf-zloc)))

(defn add-to-rcf*
  [zloc
   {libspec-type :type
    lib-sym :lib
    refer-sym :refer
    alias-sym :alias
    class-sym :class}
   _]
  (when
   (-> zloc
       edit/find-namespace
       zsub/subzip
       (need-to-add-libspec? lib-sym refer-sym alias-sym class-sym))
    (let [rcf-zip (-> zloc z/up z/subzip)
          type (-> libspec-type name)
          left-ident (or (some-> rcf-zip
                                 (z/find-tag z/next* :newline)
                                 z/next
                                 z/node
                                 meta
                                 :col
                                 dec) 2)
          col (+  left-ident 2 (count type))
        ;;look for (require ...) or (import ...)
        ;;as first or second child of comment form
          add-form-type? (not (or (= type (some-> rcf-zip
                                                  z/down
                                                  z/right
                                                  z/down
                                                  z/string))
                                  (= type (some-> rcf-zip
                                                  z/down
                                                  z/right
                                                  z/right
                                                  z/down
                                                  z/string))))
          form-to-add (cond
                        (fast= :import libspec-type)
                        [(with-meta lib-sym nil)
                         (with-meta class-sym nil)]
                        (fast= :require libspec-type)
                        (cond-> (with-meta lib-sym nil)
                          (or alias-sym refer-sym) (vector)
                          alias-sym (conj :as (with-meta alias-sym nil))
                          refer-sym (conj :refer [(with-meta refer-sym nil)]) true (#(n/quote-node [%]))))]
       ;;we can't simply return an edited comment-zipper, because the cursor
       ;;is inside and the update conflicts with completion, so we return
       ;;only the updated/inserted libform
      (if add-form-type?
        (let [range (meta (z/node rcf-zip))
              new-form (z/edit-> (z/of-string "")
                                 (z/append-child* (n/newlines 1))
                                 (cond->
                                  (> left-ident 0) (z/append-child* (n/spaces left-ident)))
                                 (z/append-child (n/list-node [(n/token-node (symbol type)) (n/spaces 1) form-to-add])))]
          [{:range {:row (:row range) :col 9 ;after (comment
                    :end-row (:row range) :end-col 9}
            :loc new-form}])
        (let [form (-> (z/find-value rcf-zip z/next (symbol type))
                       z/up)
              form-zip (z/subzip form)]
          (when (need-to-add-libspec?
                  (z/subzip form) lib-sym refer-sym alias-sym class-sym)
            (let [result-loc (or (:result-loc (modified-existing-libform form-zip form-zip lib-sym refer-sym alias-sym class-sym))
                                 (z/subedit-> form
                                              (z/append-child* (n/newlines 1))
                                              (z/append-child* (n/spaces col))
                                              (z/append-child form-to-add)))]
              [{:range (meta (z/node form))
                :loc result-loc}])))))))

(defn ^:private add-to-namespace
  [zloc type ns-sym sym db]
  (when (or (fast= :require-simple type) sym)
    (let [libspec (case type
                    :require-refer {:type :require :lib ns-sym :refer sym}
                    :require-alias {:type :require :lib ns-sym :alias sym}
                    :require-simple {:type :require :lib ns-sym}
                    :import {:type :import :lib ns-sym :class sym})]
         ;;add to rcf is zloc is comment form, otherwise add to ns
      (if (and
            zloc
            (= :token (z/tag zloc))
            (= "comment" (z/string zloc)))
        (add-to-rcf* zloc libspec db)
        (add-to-namespace* zloc libspec db)))))

(defn add-missing-import [zloc uri import-name db {:keys [producer]}]
  (when-let [import-name (or import-name
                             (first (find-missing-imports zloc db)))]
    (let [split (string/split import-name #"\.")
          package-name (symbol (string/join "." (drop-last split)))
          class-name (symbol (last split))
          rcf-zloc (add-to-rcf? zloc :import db producer)
          zloc (or rcf-zloc zloc)]
      (cond->> (add-to-namespace zloc :import package-name class-name db)
        (nil? rcf-zloc) (cleaning-ns-edits uri db)))))

(defn add-known-alias
  [zloc alias-to-add qualified-ns-to-add db]
  (when (and qualified-ns-to-add alias-to-add)
    (add-to-namespace zloc :require-alias qualified-ns-to-add alias-to-add db)))

(defn add-simple-require
  [zloc qualified-ns-to-add db]
  (when qualified-ns-to-add
    (add-to-namespace zloc :require-simple qualified-ns-to-add nil db)))

(defn ^:private add-known-refer
  [zloc refer-to-add qualified-ns-to-add db]
  (when (and qualified-ns-to-add refer-to-add)
    (add-to-namespace zloc :require-refer qualified-ns-to-add refer-to-add db)))

(defn ^:private sub-segment?
  [alias-segs def-segs]
  (loop [def-segs def-segs
         alias-segs alias-segs
         i 0
         j 0
         found-first-match? false]
    (if (empty? def-segs)
      (empty? alias-segs)
      (when-let [alias-seg (nth alias-segs i nil)]
        (if-let [def-seg (nth def-segs j nil)]
          (if (string/starts-with? def-seg alias-seg)
            (recur (subvec def-segs (inc j))
                   (subvec alias-segs (inc i))
                   0
                   0
                   true)
            (if found-first-match?
              nil
              (recur def-segs
                     alias-segs
                     i
                     (inc j)
                     found-first-match?)))
          (when found-first-match?
            (recur def-segs
                   alias-segs
                   (inc i)
                   0
                   found-first-match?)))))))

(defn ^:private resolve-best-alias-suggestions
  "Turns a namespace like clojure.data.json.internal into suggested aliases
   `[internal json.internal data.json.internal]`
   removing existing project aliases and limiting to 1 suggestion."
  [ns-str aliases->namespaces]
  (->> (string/split ns-str #"\.")
       reverse
       (drop-while #(fast= "core" %))
       (reductions
         (fn [accum ns-str]
           (str ns-str "." accum)))
       (remove aliases->namespaces)
       (remove #(= ns-str %))
       (take 1)))

(defn ^:private resolve-best-namespaces-suggestions
  [given-alias aliases->namespaces namespaces->aliases]
  (let [given-segments (string/split given-alias #"\.")
        all-definition-segments (mapv #(vec (string/split % #"\.")) (keys namespaces->aliases))]
    (->> all-definition-segments
         (filter #(sub-segment? given-segments %))
         (filter #(not (string/ends-with? (last %) "-test")))
         (sort)
         (mapcat (fn [suggested-ns-segments]
                   (let [suggested-ns (string/join "." suggested-ns-segments)]
                     (when (not (contains? aliases->namespaces suggested-ns))
                       ;; Does the ns have existing aliases
                       (if-let [aliases (->> (get namespaces->aliases suggested-ns)
                                             (map (fn [[alias n]]
                                                    {:ns suggested-ns
                                                     :alias alias
                                                     :count n}))
                                             seq)]
                         aliases
                         (let [single-segment? (= 1 (count given-segments))
                               matches-last? (= (last suggested-ns-segments) (last given-segments))
                               ns-like-search? (= (count suggested-ns-segments) (count given-segments))
                               expand-last? (and (not single-segment?) (not ns-like-search?) (not matches-last?))
                               best-alias (->> (resolve-best-alias-suggestions suggested-ns aliases->namespaces)
                                               (map (fn [suggested-alias]
                                                      {:ns suggested-ns
                                                       :alias suggested-alias
                                                       :heuristic-order (if matches-last? 0 1)})))]
                           (cond
                             single-segment?
                             best-alias

                             matches-last?
                             [{:ns suggested-ns
                               :alias given-alias
                               :heuristic-order 2}]

                             ns-like-search?
                             (concat
                               best-alias
                               [{:ns suggested-ns
                                 :heuristic-order 3}])

                             expand-last?
                             [{:ns suggested-ns
                               :alias (string/join "." (concat (butlast given-segments) [(last suggested-ns-segments)]))
                               :heuristic-order 4}])))))))
         (remove #(= (:ns %) (:alias %)))
         (sort-by (juxt :heuristic-order :ns))
         (map #(dissoc % :heuristic-order))
         distinct
         vec
         seq)))

(defn find-namespace-suggestions
  "Given a list of `[[\"ns\" \"alias\"]] pairs suggest possible requires.
   Will only suggest existing namespaces.
   Suggestions should be ordered from most commonly used to least."
  [cursor-namespace-str ns-alias-pairs]
  (let [aliases->namespaces (->> ns-alias-pairs
                                 (group-by second)
                                 (medley/map-vals (fn [vs]
                                                    (->> vs
                                                         (mapv (fn [[ns _ count _]] [ns count]))
                                                         (into (array-map))))))
        namespaces->aliases (->> ns-alias-pairs
                                 (group-by first)
                                 (medley/map-vals (fn [vs]
                                                    (->> vs
                                                         (map second)
                                                         (remove nil?)
                                                         frequencies
                                                         (sort-by (fn [[alias freq]]
                                                                    [(- freq) alias]))
                                                         (into (array-map))))))
        js-requires (->> ns-alias-pairs
                         (keep (fn [[alias-namespace _ _ original-alias-namespace]]
                                 (when (string? original-alias-namespace)
                                   alias-namespace)))
                         set)
        alias-namespaces (get aliases->namespaces cursor-namespace-str)
        namespace-aliases (get namespaces->aliases cursor-namespace-str)
        common-namespace (some-> (get common-sym/common-alias->info (symbol cursor-namespace-str)) str)
        common-aliases (seq (get namespaces->aliases common-namespace))]
    (cond
      ;; This alias is used in the project, so suggest the aliased namespaces
      alias-namespaces
      (->> alias-namespaces
           (map (fn [[alias-namespace ns-count]]
                  (cond-> {:ns alias-namespace
                           :alias cursor-namespace-str
                           :count ns-count}
                    ;; Written like this just to keep old test cases working
                    (contains? js-requires alias-namespace) (assoc :js-require true)))))

      ;; This namespace has existing aliases
      (and namespace-aliases (seq namespace-aliases))
      (->> namespace-aliases
           (mapv (fn [[namespace-alias alias-count]]
                   (cond-> {:ns cursor-namespace-str
                            :alias namespace-alias
                            :count alias-count}
                     (contains? js-requires cursor-namespace-str) (assoc :js-require true)))))

      ;; This is not an existing alias, so assume an existing, fully qualified namespace
      ;; Derive suggestions for new aliases
      namespace-aliases
      (conj
        (->> (resolve-best-alias-suggestions cursor-namespace-str aliases->namespaces)
             (mapv (fn [suggestion]
                     {:ns cursor-namespace-str
                      :alias suggestion})))
        {:ns cursor-namespace-str})

      ;; Common namespace alias, aliased as something else
      (and common-namespace common-aliases)
      (->> common-aliases
           (mapv (fn [[common-alias n]]
                   {:ns common-namespace
                    :alias common-alias
                    :count n})))

      ;; Common namespace alias, not aliased as something else
      common-namespace
      [{:ns common-namespace
        :alias cursor-namespace-str}
       {:ns common-namespace}]

      ;; not a valid namespace maybe trying to fuzzy match like `c.f.a`)
      :else
      (resolve-best-namespaces-suggestions cursor-namespace-str aliases->namespaces namespaces->aliases))))

(defn find-alias-ns-pairs [db uri]
  (let [langs (shared/uri->available-langs uri)]
    ;; Fourth value to keep the original name type so we can check for JS requires
    (concat (->> (dep-graph/ns-aliases-for-langs db langs)
                 (map (juxt (comp str :to) (comp str :alias) :usages-count :to)))
            (->> (dep-graph/ns-names-for-langs db langs)
                 (map (juxt str (constantly nil) (constantly nil) identity))))))

(defn find-require-suggestions [zloc uri db]
  (when-let [cursor-sym (safe-sym zloc)]
    (let [cursor-namespace-str (namespace cursor-sym)
          cursor-name-str (name cursor-sym)
          namespace-suggestions (find-namespace-suggestions
                                  (or cursor-namespace-str cursor-name-str)
                                  (find-alias-ns-pairs db uri))
          uri-nses (->> (get-in db [:analysis uri :namespace-definitions])
                        (map :name)
                        set)
          suggestions (if (namespace cursor-sym)
                        namespace-suggestions
                        (concat
                          namespace-suggestions
                          (if-let [common-refer (get common-sym/common-refers->info (symbol cursor-name-str))]
                            [{:ns (name common-refer)
                              :refer cursor-name-str}]
                            (into []
                                  (comp
                                    (remove #(uri-nses (:ns %)))
                                    (filter #(= cursor-name-str (str (:name %))))
                                    (map (fn [element]
                                           {:ns (str (:ns element))
                                            :refer cursor-name-str})))
                                  (q/find-all-var-definitions db)))))]
      suggestions)))

(defn ^:private find-forms [zloc p?]
  (->> zloc
       (iterate z/next)
       (take-while (complement z/end?))
       (filter p?)))

(defn add-ns-to-loc-change [loc chosen-alias]
  (let [replaced-loc (-> loc
                         (z/replace (-> (symbol (name chosen-alias) (-> loc safe-sym name))
                                        n/token-node
                                        (with-meta (meta (z/node loc))))))]
    {:range (meta (z/node replaced-loc))
     :loc replaced-loc}))

(defn add-require-suggestion [zloc uri chosen-ns chosen-alias chosen-refer js-require db {:keys [producer]}]
  (when-let [cursor-sym (safe-sym zloc)]
    (let [chosen-ns (if js-require
                      chosen-ns
                      (symbol chosen-ns))
          cursor-namespace-str (namespace cursor-sym)
          chosen-alias-or-ns (when-not chosen-refer (or chosen-alias chosen-ns))
          rcf-zloc (add-to-rcf? zloc :require db producer)
          to-ns? (nil? rcf-zloc)
          zloc (or rcf-zloc zloc)]
      (->> (concat
             (cond->> (cond
                        chosen-refer
                        (add-known-refer zloc (symbol chosen-refer) chosen-ns db)

                        chosen-alias
                        (add-known-alias zloc (symbol chosen-alias-or-ns) chosen-ns db)

                        :else
                        (add-simple-require zloc chosen-ns db))
               to-ns? (cleaning-ns-edits uri db))
             (when chosen-alias-or-ns
               (cond
                 cursor-namespace-str
                 ;; When we're aliasing clojure.string to string, we want to change
                 ;; all nodes after the namespace like clojure.string/split to string/split.
                 (->> (find-forms (if to-ns?
                                    (z/next (edit/find-namespace zloc))
                                    (-> zloc z/up z/subzip))
                                  #(when-let [sym-ns (some-> % safe-sym namespace)]
                                     (and (or
                                            (= chosen-ns sym-ns)
                                            (= cursor-namespace-str sym-ns))
                                          (not= chosen-alias-or-ns sym-ns))))
                      (mapv #(add-ns-to-loc-change % chosen-alias-or-ns)))

                 (and to-ns?
                      (some-> zloc safe-sym)
                      ;; It seems like it isn't useful to modify the cursor-symbol
                      ;; when adding a JS require, because the completion result is only given
                      ;; when the cursor symbol is already using the selected alias?
                      (not js-require))
                 [(add-ns-to-loc-change zloc chosen-alias-or-ns)])))
           seq))))

(defn add-missing-libspec
  [zloc uri db components]
  (when zloc
    (let [all-suggestions (find-require-suggestions zloc uri db)]
      (when-let [suggestion (some->> all-suggestions
                                     seq
                                     first)]
        (add-require-suggestion
          zloc
          uri
          (:ns suggestion)
          (:alias suggestion)
          (:refer suggestion)
          (:js-require suggestion)
          db
          components)))))

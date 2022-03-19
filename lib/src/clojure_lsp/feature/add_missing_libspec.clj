(ns clojure-lsp.feature.add-missing-libspec
  (:require
   [clojure-lsp.common-symbols :as common-sym]
   [clojure-lsp.queries :as q]
   [clojure-lsp.refactor.edit :as edit]
   [clojure-lsp.settings :as settings]
   [clojure-lsp.shared :as shared]
   [clojure.set :as set]
   [clojure.string :as string]
   [medley.core :as medley]
   [rewrite-clj.node :as n]
   [rewrite-clj.zip :as z]
   [rewrite-clj.zip.subedit :as zsub]))

(set! *warn-on-reflection* true)

(defn safe-sexpr [zloc]
  (when (z/sexpr-able? zloc)
    (z/sexpr zloc)))

(defn safe-sym [zloc]
  (when-let [s (safe-sexpr zloc)]
    (when (symbol? s)
      s)))

(defn ^:private resolve-ns-inner-blocks-identation [db]
  (or (settings/get db [:clean :ns-inner-blocks-indentation])
      (if (settings/get db [:keep-require-at-start?])
        :same-line
        :next-line)))

(defn ^:private find-missing-ns-alias-require [zloc db]
  (let [require-alias (some-> zloc safe-sym namespace symbol)
        alias->info (->> (q/find-all-aliases (:analysis @db))
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
  [zloc db]
  (if (some-> zloc safe-sym namespace)
    (find-missing-ns-alias-require zloc db)
    (find-missing-ns-refer-require zloc)))

(defn ^:private find-class-name [zloc]
  (when-let [sym (safe-sym zloc)]
    (let [value (z/string zloc)]
      (cond
        (string/ends-with? value ".")
        (->> value drop-last (string/join "") symbol)

        (namespace sym)
        (-> sym namespace symbol)

        :else sym))))

(defn find-missing-import [zloc]
  (->> zloc
       find-class-name
       (get common-sym/java-util-imports)))

(defn ^:private add-to-namespace
  [zloc type ns sym db]
  (let [cursor-sym (safe-sym zloc)]
    (when (and sym cursor-sym)
      (let [form-type    (case type
                           :require-refer :require
                           :require-alias :require
                           :import        :import)
            ns-loc       (edit/find-namespace zloc)
            ns-zip       (zsub/subzip ns-loc)
            need-to-add? (and (not (z/find-value ns-zip z/next cursor-sym))
                              (or ns
                                  (not (z/find-value ns-zip z/next sym)))
                              (or (not (z/find-value ns-zip z/next ns))
                                  (not (z/find-value ns-zip z/next sym))))]
        (when need-to-add?
          (let [add-form-type?              (not (z/find-value ns-zip z/next form-type))
                form-type-loc               (z/find-value (zsub/subzip ns-loc) z/next form-type)
                ns-inner-blocks-indentation (resolve-ns-inner-blocks-identation db)
                col                         (if form-type-loc
                                              (-> form-type-loc z/rightmost z/node meta :col)
                                              (if (= :same-line ns-inner-blocks-indentation)
                                                2
                                                5))
                form-to-add                 (case type
                                              :require-refer [ns :refer [sym]]
                                              :require-alias [ns :as sym]
                                              :import        sym)
                existing-refer-ns           (and (= type :require-refer)
                                                 (z/find-value ns-zip z/next ns))
                existing-require-refer      (when existing-refer-ns
                                              (z/find-value existing-refer-ns z/next ':refer))

                result-loc (if existing-refer-ns
                             (if existing-require-refer
                               (z/subedit-> ns-zip
                                            (z/find-value z/next ns)
                                            (z/find-value z/next ':refer)
                                            z/right
                                            (z/append-child* (n/spaces 1))
                                            (z/append-child sym))
                               (z/subedit-> ns-zip
                                            (z/find-value z/next ns)
                                            z/up
                                            (z/append-child* (n/spaces 1))
                                            (z/append-child :refer)
                                            (z/append-child [sym])))
                             (z/subedit-> ns-zip
                                          (cond->
                                           add-form-type? (z/append-child (n/newlines 1))
                                           add-form-type? (z/append-child (n/spaces 2))
                                           add-form-type? (z/append-child (list form-type)))
                                          (z/find-value z/next form-type)
                                          (z/up)
                                          (cond->
                                           (or (not add-form-type?)
                                               (= :next-line ns-inner-blocks-indentation)) (z/append-child* (n/newlines 1)))
                                          (z/append-child* (n/spaces (dec col)))
                                          (z/append-child form-to-add)))]
            [{:range (meta (z/node result-loc))
              :loc result-loc}]))))))

(defn add-import-to-namespace [zloc import-name db]
  (add-to-namespace zloc :import nil (symbol import-name) db))

(defn add-common-import-to-namespace [zloc db]
  (when-let [import-name (find-missing-import zloc)]
    (add-import-to-namespace zloc import-name db)))

(defn add-known-alias
  [zloc alias-to-add qualified-ns-to-add db]
  (when (and qualified-ns-to-add alias-to-add)
    (add-to-namespace zloc :require-alias qualified-ns-to-add alias-to-add db)))

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
       (drop-while #(= "core" %))
       (reductions
         (fn [accum ns-str]
           (str ns-str "." accum)))
       (remove aliases->namespaces)
       (remove #(= ns-str %))
       (take 1)))

(defn ^:private resolve-best-namespaces-suggestions
  [alias-str aliases->namespaces namespaces->aliases]
  (let [alias-segments (string/split alias-str #"\.")
        all-definition-segments (map #(string/split % #"\.") (keys namespaces->aliases))]
    (->> all-definition-segments
         (filter #(sub-segment? alias-segments %))
         (filter #(not (string/ends-with? (last %) "-test")))
         (map #(string/join "." %))
         (remove aliases->namespaces)
         (mapcat (fn [suggested-ns]
                   ;; Does the ns have existing aliases
                   (if-let [aliases (->> (get namespaces->aliases suggested-ns)
                                         (map (fn [[alias n]]
                                                {:alias alias
                                                 :ns suggested-ns
                                                 :count n}))
                                         seq)]
                     aliases
                     ;; Can we generate good alias suggestions for the found namespace
                     (if-let [alias-suggestions (->> (resolve-best-alias-suggestions suggested-ns aliases->namespaces)
                                                     (map (fn [suggested-alias]
                                                            {:alias suggested-alias
                                                             :ns suggested-ns}))
                                                     seq)]
                       alias-suggestions
                       ;; We found it so use the given alias as last resort
                       [{:alias alias-str
                         :ns suggested-ns}]))))
         seq)))

(defn ^:private find-namespace-suggestions
  "Given a list of `[[\"ns\" \"alias\"]] pairs suggest possible requires.
   Will only suggest existing namespaces.
   Suggestions should be ordered from most commonly used to least."
  [cursor-namespace-str ns-alias-pairs]
  (let [aliases->namespaces (->> ns-alias-pairs
                                 (group-by second)
                                 (medley/map-vals (fn [vs]
                                                    (->> vs
                                                         (map first)
                                                         frequencies
                                                         (sort-by val)
                                                         reverse
                                                         (into (array-map))))))
        namespaces->aliases (->> ns-alias-pairs
                                 (group-by first)
                                 (medley/map-vals (fn [vs]
                                                    (->> vs
                                                         (map second)
                                                         (remove nil?)
                                                         frequencies
                                                         (sort-by val)
                                                         reverse
                                                         (into (array-map))))))
        alias-namespaces (get aliases->namespaces cursor-namespace-str)
        namespace-aliases (get namespaces->aliases cursor-namespace-str)
        common-namespace (some-> (get common-sym/common-alias->info (symbol cursor-namespace-str)) str)
        common-aliases (seq (get namespaces->aliases common-namespace))]
    (cond
      ;; This alias is used in the project, so suggest the aliased namespaces
      alias-namespaces
      (->> alias-namespaces
           (map (fn [[alias-namespace ns-count]]
                  {:ns alias-namespace
                   :alias cursor-namespace-str
                   :count ns-count})))

      ;; This namespace has existing aliases
      (and namespace-aliases (seq namespace-aliases))
      (->> namespace-aliases
           (mapv (fn [[namespace-alias alias-count]]
                   {:ns cursor-namespace-str
                    :alias namespace-alias
                    :count alias-count})))

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


(defn find-require-suggestions [zloc uri db]
  (when-let [cursor-sym (safe-sym zloc)]
    (let [cursor-namespace-str (namespace cursor-sym)
          cursor-name-str (name cursor-sym)
          analysis (:analysis @db)
          langs (shared/uri->available-langs uri)
          all-aliases (->> (q/find-all-aliases analysis)
                           (filter (fn [element]
                                     (seq (set/intersection (-> element
                                                                :filename
                                                                (shared/filename->uri db)
                                                                shared/uri->available-langs)
                                                       langs))))
                           (map (juxt (comp str :to) (comp str :alias))))
          ns-definition-names (->> (q/find-all-ns-definition-names analysis)
                                   (map (juxt str (constantly nil))))
          alias-ns-pairs (concat all-aliases ns-definition-names)
          namespace-suggestions (find-namespace-suggestions
                                  (or cursor-namespace-str cursor-name-str)
                                  alias-ns-pairs)
          suggestions (if (namespace cursor-sym)
                        namespace-suggestions
                        (concat
                          namespace-suggestions
                          (if-let [common-refer (get common-sym/common-refers->info (symbol cursor-name-str))]
                            [{:ns (name common-refer)
                              :refer cursor-name-str}]
                            (->> (q/find-all-var-definitions analysis)
                                 (filter #(= cursor-name-str (str (:name %))))
                                 (map (fn [element]
                                        {:ns (str (:ns element))
                                         :refer cursor-name-str}))))))]
      suggestions)))

(defn ^:private find-forms [zloc p?]
  (->> zloc
       (iterate z/next)
       (take-while (complement z/end?))
       (filter p?)))

(defn add-ns-to-loc-change [loc chosen-alias]
  (let [replaced-loc (-> loc
                         (z/replace (-> (symbol chosen-alias (-> loc safe-sym name))
                                        n/token-node
                                        (with-meta (meta (z/node loc))))))]
    {:range (meta (z/node replaced-loc))
     :loc replaced-loc}))

(defn add-require-suggestion [zloc chosen-ns chosen-alias chosen-refer db]
  (when-let [cursor-sym (safe-sym zloc)]
    (let [cursor-namespace-str (namespace cursor-sym)]
      (seq
        (if chosen-alias
          (concat (add-known-alias zloc (symbol chosen-alias) (symbol chosen-ns) db)
                  (cond
                    cursor-namespace-str
                    ;; When we're aliasing clojure.string to string, we want to change
                    ;; all nodes after the namespace like clojure.string/split to string/split.
                    (->> (find-forms (z/next (edit/find-namespace zloc))
                                     #(when-let [sym-ns (some-> % safe-sym namespace)]
                                        (and (or
                                               (= chosen-ns sym-ns)
                                               (= cursor-namespace-str sym-ns))
                                             (not= chosen-alias sym-ns))))
                         (map #(add-ns-to-loc-change % chosen-alias)))

                    (some-> zloc safe-sym)
                    [(add-ns-to-loc-change zloc chosen-alias)]))
          (add-known-refer zloc (symbol chosen-refer) (symbol chosen-ns) db))))))

(defn add-missing-libspec
  [zloc uri db]
  (when zloc
    (let [all-suggestions (find-require-suggestions zloc uri db)]
      (when-let [suggestion (some->> all-suggestions
                              seq
                              first)]
        (add-require-suggestion
          zloc
          (:ns suggestion)
          (:alias suggestion)
          (:refer suggestion)
          db)))))

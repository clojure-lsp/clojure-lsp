(ns clojure-lsp.feature.add-missing-libspec
  (:require
   [clojure-lsp.common-symbols :as common-sym]
   [clojure-lsp.queries :as q]
   [clojure-lsp.refactor.edit :as edit]
   [clojure-lsp.settings :as settings]
   [clojure.string :as string]
   [medley.core :as medley]
   [rewrite-clj.node :as n]
   [rewrite-clj.zip :as z]
   [rewrite-clj.zip.subedit :as zsub]
   [taoensso.timbre :as log]))

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

(defn ^:private add-missing-alias-ns [zloc db]
  (let [require-alias (some-> zloc safe-sym namespace symbol)
        qualified-ns-to-add (:ns (find-missing-ns-alias-require zloc db))]
    (add-known-alias zloc require-alias qualified-ns-to-add db)))

(defn ^:private add-missing-refer-ns [zloc db]
  (let [require-refer (some-> zloc safe-sym)
        qualified-ns-to-add (:ns (find-missing-ns-refer-require zloc))]
    (add-known-refer zloc require-refer qualified-ns-to-add db)))

(defn add-missing-libspec
  [zloc db]
  (when-let [sym (some-> zloc safe-sym)]
    (if (namespace sym)
      (add-missing-alias-ns zloc db)
      (add-missing-refer-ns zloc db))))

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

(defn ^:private resolve-best-namespaces-suggestions
  [alias-str ns-definitions]
  (let [alias-segments (string/split alias-str #"\.")
        all-definition-segments (map (comp #(string/split % #"\.") str) ns-definitions)]
    (->> all-definition-segments
         (filter #(sub-segment? alias-segments %))
         (filter #(not (string/ends-with? (last %) "-test")))
         (map #(string/join "." %))
         set)))

(defn ^:private resolve-best-alias-suggestion
  [ns-str all-aliases drop-core?]
  (if-let [dot-index (string/last-index-of ns-str ".")]
    (let [suggestion (subs ns-str (inc dot-index))]
      (if (and drop-core?
               (= "core" suggestion))
        (resolve-best-alias-suggestion (subs ns-str 0 dot-index) all-aliases drop-core?)
        suggestion))
    ns-str))

(defn ^:private resolve-best-alias-suggestions
  [ns-str all-aliases]
  (let [alias (resolve-best-alias-suggestion ns-str all-aliases true)]
    (if (contains? all-aliases (symbol alias))
      (if-let [dot-index (string/last-index-of ns-str ".")]
        (let [ns-without-alias (subs ns-str 0 dot-index)
              second-alias-suggestion (resolve-best-alias-suggestion ns-without-alias all-aliases false)]
          (if (= second-alias-suggestion alias)
            #{alias}
            (conj #{alias}
                  (str second-alias-suggestion "." alias))))
        #{alias})
      #{alias})))

(defn ^:private find-alias-require-suggestions [alias-str missing-requires db]
  (let [analysis (:analysis @db)
        ns-definitions (q/find-all-ns-definition-names analysis)
        all-aliases (->> (q/find-all-aliases analysis)
                         (map :alias)
                         set)]
    (cond->> []

      (contains? ns-definitions (symbol alias-str))
      (concat
        (->> (resolve-best-alias-suggestions alias-str all-aliases)
             (map (fn [suggestion]
                    {:ns alias-str
                     :alias suggestion}))))

      (not (contains? ns-definitions (symbol alias-str)))
      (concat (->> (resolve-best-namespaces-suggestions alias-str ns-definitions)
                   (map (fn [suggestion]
                          {:ns suggestion
                           :alias alias-str}))))

      :always
      (remove (fn [suggestion]
                (some #(= (str (:ns %))
                          (str (:ns suggestion)))
                      missing-requires))))))

(defn ^:private find-refer-require-suggestions [refer missing-requires db]
  (let [analysis (:analysis @db)
        all-valid-refers (->> (q/find-all-var-definitions analysis)
                              (filter #(= refer (:name %))))]
    (cond->> []
      (seq all-valid-refers)
      (concat (->> all-valid-refers
                   (map (fn [element]
                          {:ns (str (:ns element))
                           :refer (str refer)}))))
      :always
      (remove (fn [element]
                (some #(= (str (:ns %))
                          (str (:ns element)))
                      missing-requires))))))

(defn find-require-suggestions [zloc missing-requires db]
  (when-let [sym (safe-sym zloc)]
    (if-let [alias-str (namespace sym)]
      (find-alias-require-suggestions alias-str missing-requires db)
      (find-refer-require-suggestions sym missing-requires db))))

(defn add-require-suggestion [zloc chosen-ns chosen-alias chosen-refer db]
  (let [;; When this node is clojure.string/split and we are aliasing
        ;; clojure.string to my-str, we want to change this node to
        ;; my-str/split. The code actions never suggest this (they always
        ;; suggest clojure.string), but it's possible if the command is invoked
        ;; directly.
        ns-usages-nodes (edit/find-forms zloc #(when-let [sym (safe-sym %)]
                                                 (and (= chosen-ns (namespace sym))
                                                      (not= chosen-alias (namespace sym)))))
        chosen-require  (if chosen-alias
                          (add-known-alias zloc (symbol chosen-alias) (symbol chosen-ns) db)
                          (add-known-refer zloc (symbol chosen-refer) (symbol chosen-ns) db))]
    (seq
      (concat chosen-require
              (when chosen-alias
                (->> ns-usages-nodes
                     (map (fn [node]
                            (z/replace node (-> (symbol chosen-alias (-> node z/sexpr name))
                                                n/token-node
                                                (with-meta (meta (z/node  node)))))))
                     (map (fn [loc]
                            {:range (meta (z/node loc))
                             :loc   loc}))))))))

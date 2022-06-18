(ns clojure-lsp.dep-graph
  (:require
   [clojure-lsp.shared :as shared]
   [clojure.set :as set]))

;;;; Syntax

;; () grouping
;; | choice
;; ? zero or one
;; + one or more
;; * zero or more
;; '' literal
;; "" string
;; [] = list or vector
;; {} = map {k1 v1 ...}
;; #{} = set #{i1 ...}
;; #<> = multiset #<i1 ...>
;;       a mulitset is represented internally as a hashmap {i1 f1 ...} where f1 =
;;       integer > 0, frequency of i1

;; db = {':dep-graph' dep-graph
;;       ':file-meta' file-meta}

;; dep-graph = {(ns dep-graph-item)*}
;; ns = symbol
;; dep-graph-item =
;; {(':dependencies' #<ns*>)?
;;  (':dependents' #<ns*>)?
;;  (':aliases' #<alias*>)?
;;  (':files' #{filename*})?
;;  (':internal?' boolean)?
;;  (':from-internal?' boolean)?
;;  (':from-langs' #{lang*})?}
;; filename = string
;; alias = (symbol | nil)
;; lang = ':clj' | ':cljs' | ':edn'

;; :dependencies is a multiset of the namespaces that this namespace depends on.
;; It's a multiset because a dependency may be required several times, either
;; with different aliases, or because the dependent namespace is defined across
;; several files. We keep a multiset because removing one of these duplicates
;; doesn't erase the dependency/dependent relationship. If a namespace has no
;; dependencies, the :dependencies will either be absent from the
;; dep-graph-item, or be an empty multiset. The system doesn't make guarantees
;; about which it will be.

;; :dependents is a multiset of the namespaces that depend on this namespace.
;; May be either empty or absent if namespace isn't depended on by other
;; namespaces.

;; If the code had `(ns aaa (require [bbb :as b]))`, then `'bbb` would be in
;; `'aaa`'s :dependencies, and `'aaa` would be in `'bbb`'s :dependents.

;; :aliases is a multiset of the symbols that are used to refer to this
;; namespace. If the code had `(:require [aaa :as a])`, then `'aaa` would
;; include `'a` in its aliases. Unaliased requires like `(:require [aaa])` will
;; include `nil` in their aliases. May be either empty or absent if a namespace
;; isn't required by other namespaces.

;; :files is a set of the files in which the namespace is defined. This can be
;; either absent or empty if the namespace is required but never defined, either
;; as a syntax error or via :as-alias.

;; :internal? is true when the namespace is defined in any internal file, falsy
;; otherwise. May be absent if the namespace is required but never defined, as
;; for :files.

;; :from-internal? is true when the namespace is required by any internal
;; namespace, falsy otherwise. May be absent if the namespace isn't required by
;; other namespaces.

;; :from-langs is a set of the langs of the :namespace-usages or files (if the
;; :namespace-usages don't have langs) that use this namespace. May be either
;; empty or absent if the namespace isn't required by other namespaces.

;; file-meta = {(filename file-meta-item)*}
;; file-meta-item =
;; {(:namespaces #{ns*})?
;;  :internal? boolean
;;  :langs #{lang+}}

;; :namespaces is a set of the namespaces defined by this file. May be either
;; empty or absent if the file doesn't define any namespaces.

;; :internal? is whether this file is internal to the project.

;; :langs is a set of the langs used by this file.

;;;; DB Maintenance

(def ^:private empty-multiset {})

(defn ^:private mulitset-conj [ms x]
  (update ms x (fnil inc 0)))

(defn ^:private multiset-disj [ms x]
  (if-let [v (get ms x)]
    (let [new-v (dec v)]
      (if (zero? new-v)
        (dissoc ms x)
        (assoc ms x new-v)))
    ms))

(defn ms-distinct [ms] (some-> ms keys))

(defn ms-overlaps-set? [ms s]
  ;; Conceptually set overlap is symmetric, but with our implementation of
  ;; multiset, in practice we must check whether the items of the set are in the
  ;; multiset, not vice-versa.
  (boolean (and ms (some ms s))))

(def ^:private ms-conj (fnil mulitset-conj empty-multiset))
(def ^:private ms-disj (fnil multiset-disj empty-multiset))
(def ^:private s-conj (fnil conj #{}))
(def ^:private s-disj (fnil disj #{}))

(defn file-internal? [db filename]
  (get-in db [:file-meta filename :internal?]))

(defn ^:private update-usage [db f {:keys [from name alias filename] :as element}]
  (let [from-file (get-in db [:file-meta filename])
        from-langs (or (some-> element :lang list set) (:langs from-file))]
    ;; A dep-graph item is a summary of the ways a namespace is used across the
    ;; whole project. We keep multisets of most of its data, so that we know not
    ;; only that a namespace `aaa` is aliased as `a`, but that it is aliased as
    ;; `a` 7 times. If the alias is removed from a single file we decrement to
    ;; 6, but we don't lose track of the fact that it's still aliased as `a`
    ;; somewhere else.
    (-> db
        (update-in [:dep-graph from :dependencies] f name)
        (update-in [:dep-graph name :dependents] f from)
        (update-in [:dep-graph name :aliases] f alias)
        ;; NOTE: We could store :from-filenames, and look up whether any of them
        ;; are internal. But this way keeps that lookup out of ns-aliases, which
        ;; is on the hotpath in completion. :from-langs is only used in
        ;; add-missing-libspec, so it's not on a hotpath, but we've already
        ;; established this pattern.
        ;; TODO: is it wrong that once from-internal?, always from-internal?
        (update-in [:dep-graph name :from-internal?] #(or % (:internal? from-file)))
        (update-in [:dep-graph name :from-langs] #(reduce f % from-langs)))))

(defn ^:private update-usages [db f namespace-usages]
  (reduce #(update-usage %1 f %2) db namespace-usages))

(defn ^:private remove-usages [db usages] (update-usages db ms-disj usages))
(defn ^:private add-usages [db usages]    (update-usages db ms-conj usages))

(defn ^:private update-definition [db f {:keys [name filename]}]
  (let [in-file (get-in db [:file-meta filename])]
    (-> db
        (update-in [:dep-graph name :files] f filename)
        ;; TODO: is it wrong that once an internal ns, always an internal ns?
        (update-in [:dep-graph name :internal?] #(or % (:internal? in-file)))
        (update-in [:file-meta filename :namespaces] f name))))

(defn ^:private update-definitions [db f namespace-definitions]
  (reduce #(update-definition %1 f %2) db namespace-definitions))

(defn ^:private remove-definitions [db namespace-definitions] (update-definitions db s-disj namespace-definitions))
(defn ^:private add-definitions [db namespace-definitions]    (update-definitions db s-conj namespace-definitions))

(defn ^:private ensure-file [db filename internal?]
  (let [uri (shared/filename->uri filename db)]
    ;; TODO: consider using file-meta as a cache for other things: uri,
    ;; file-type, etc.
    (update-in db [:file-meta filename] assoc
               :internal? internal?
               :langs (shared/uri->available-langs uri))))

(defn ^:private ensure-files [db filenames internal?]
  (reduce #(ensure-file %1 %2 internal?) db filenames))

(def ^:private user-ns-def
  ;; add :filename
  '{:bucket :namespace-definitions
    :name user})

(def ^:private clojure-core-ns-usage
  ;; add :filename and :from ns
  '{:bucket :namespace-usages
    :name clojure.core})

(def ^:private cljs-core-ns-usage
  ;; add :filename and :from ns
  '{:bucket :namespace-usages
    :name cljs.core})

(defn ^:private ns-definitions-and-usages [analysis]
  (letfn [(result-with-element [result element]
            (let [result (if (= 'user (:ns element))
                           (assoc result :in-user true)
                           result)]
              (case (:bucket element)
                :namespace-definitions (update result :defs conj element)
                :namespace-usages      (update result :usages conj element)
                :var-usages            (case (:to element)
                                         clojure.core (update result :to-clj conj (:from element))
                                         cljs.core    (update result :to-cljs conj (:from element))
                                         result)
                result)))]
    (let [{:keys [defs usages]}
          (reduce-kv (fn [result filename elements]
                       (let [result (reduce result-with-element
                                            (assoc result
                                                   :in-user false
                                                   :to-clj #{}
                                                   :to-cljs #{})
                                            elements)
                             implicit-elements (concat
                                                 (when (:in-user result)
                                                   [(assoc user-ns-def :filename filename)])
                                                 (map (fn [from-ns]
                                                        (assoc clojure-core-ns-usage
                                                               :from from-ns
                                                               :filename filename))
                                                      (:to-clj result))
                                                 (map (fn [from-ns]
                                                        (assoc cljs-core-ns-usage
                                                               :from from-ns
                                                               :filename filename))
                                                      (:to-cljs result)))]
                         (reduce result-with-element result implicit-elements)))
                     {:defs [] :usages []}
                     analysis)]
      [defs usages])))

(defn refresh-analysis [db old-analysis new-analysis internal?]
  ;; TODO: should this be wrapped in use-dep-graph? If it is, startup will be
  ;; faster (500ms in medium projects 1200ms in v. large). But if it isn't, you
  ;; can toggle the setting live.
  (shared/logging-task
    :maintain-dep-graph
    (let [[old-definitions old-usages] (ns-definitions-and-usages old-analysis)
          [new-definitions new-usages] (ns-definitions-and-usages new-analysis)]
      (-> db
          (ensure-files (keys new-analysis) internal?)
          (remove-usages old-usages)
          (add-usages new-usages)
          (remove-definitions old-definitions)
          (add-definitions new-definitions)))))

(defn remove-file [db filename]
  (-> db
      (refresh-analysis (select-keys (:analysis db) [filename])
                        {}
                        (file-internal? db filename))
      (update :file-meta dissoc filename)))

;;;; File filtering

(def internal-xf ;; works for dep-graph or file-meta
  (filter (comp :internal? val)))

(def external-xf ;; works for dep-graph or file-meta
  (remove (comp :internal? val)))

(def from-internal-xf ;; works for dep-graph only
  (filter (comp :from-internal? val)))

(defn ns-files [{:keys [dep-graph]} namespace]
  (get-in dep-graph [namespace :files]))

(defn ns-internal-files [db namespace]
  (filter #(file-internal? db %) (ns-files db namespace)))

(defn ns-dependents-files [{:keys [dep-graph] :as db} namespace]
  (let [dependents (get-in dep-graph [namespace :dependents])]
    (into #{}
          (mapcat #(ns-files db %))
          (ms-distinct dependents))))

(defn ns-dependencies-files [{:keys [dep-graph] :as db} namespace]
  (let [dependencies (get-in dep-graph [namespace :dependencies])]
    (into #{}
          (mapcat #(ns-files db %))
          (ms-distinct dependencies))))

(defn ns-and-dependents-files [db namespace]
  (set/union (ns-files db namespace)
             (ns-dependents-files db namespace)))

(defn nses-files [db namespaces]
  (apply set/union
         (map #(ns-files db %)
              namespaces)))

(defn nses-and-dependents-files [db namespaces]
  (apply set/union
         (map #(ns-and-dependents-files db %)
              namespaces)))

(defn internal-files [{:keys [file-meta]}]
  (into []
        (comp internal-xf (map key))
        file-meta))

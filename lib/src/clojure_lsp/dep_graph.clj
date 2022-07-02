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
;; {} map {k1 v1 ...}
;; #{} set #{i1 ...}
;; #<> multiset #<i1 ...>
;;     a mulitset is represented internally as a hashmap {i1 f1 ...} where f1 =
;;     integer > 0, frequency of i1

;; ns = symbol
;; uri = string
;; alias = symbol
;; lang = ':clj' | ':cljs' | ':edn'

;; dep-graph-item =
;; {(':dependencies' #<ns*>)?
;;  (':dependents' #<ns*>)?
;;  (':aliases' #<(alias | nil)*>)? ;; nil for when the ns is required without an alias
;;  (':uris' #{uri*})?
;;  (':internal?' boolean)?
;;  (':dependents-internal?' boolean)?
;;  (':dependents-langs' #<lang*>)?}

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

;; :uris is a set of the uris of the files in which the namespace is defined.
;; This can be either absent or empty if the namespace is required but never
;; defined, either as a syntax error or via :as-alias.

;; :internal? is true when the namespace is defined in any internal file, falsy
;; otherwise. May be absent if the namespace is required but never defined, as
;; for :uris.

;; :dependents-internal? is true when any of the namespace's dependents are
;; internal, falsy otherwise. May be absent if the namespace isn't required by
;; other namespaces.

;; :dependents-langs is a multiset of the langs of the :namespace-usages or
;; files (if the :namespace-usages don't have langs) that use this namespace.
;; May be either empty or absent if the namespace isn't required by other
;; namespaces.

;; documents-item =
;; {(:namespaces #{ns*})?
;;  :internal? boolean
;;  :langs #{lang+}}

;; :namespaces is a set of the namespaces defined by this document. May be
;; either empty or absent if the document doesn't define any namespaces.

;; :internal? is whether this document is internal to the project.

;; :langs is a set of the langs used by this document.

;; dep-graph = {(ns dep-graph-item)*}
;; documents = {(uri documents-item)*}
;; file-meta = {(filename {:uri uri})*}
;; db = {':dep-graph' dep-graph
;;       ':documents' documents
;;       ':file-meta' file-meta}

(comment
  ;; Example
  (require '[clojure-lsp.db :as db])
  (-> @db/db*
      :dep-graph
      (select-keys '[clojure-lsp.main
                     clojure.tools.cli]))
  '{clojure-lsp.main  {:aliases              {main 1}
                       :dependencies         {borkdude.dynaload        1
                                              clojure-lsp.internal-api 1
                                              clojure-lsp.kondo        1
                                              clojure-lsp.server       1
                                              clojure-lsp.shared       1
                                              clojure.core             1
                                              clojure.edn              1
                                              clojure.java.io          1
                                              clojure.string           1
                                              clojure.tools.cli        1
                                              pod.clojure-lsp.api      1}
                       :dependents           {clojure-lsp.main-test 1}
                       :dependents-internal? true
                       :dependents-langs     {:clj 1}
                       :internal?            true
                       :uris                 #{"file:///path/to/code/clojure-lsp/cli/src/clojure_lsp/main.clj"}}
    clojure.tools.cli {:aliases              {cli 7, nil 1}
                       :dependencies         {clojure.string 2, goog.string.format 1}
                       :dependents           {clj-depend.main                                    1
                                              cljfmt.main                                        1
                                              clojure-lsp.main                                   1
                                              clojure.tools.deps.alpha.script.generate-manifest2 1
                                              clojure.tools.deps.alpha.script.make-classpath2    1
                                              clojure.tools.deps.alpha.script.print-tree         1
                                              clojure.tools.deps.alpha.script.resolve-tags       1
                                              kaocha.runner                                      1}
                       :dependents-internal? true
                       :dependents-langs     {:clj 8}
                       :internal?            false
                       :uris                 #{"jar:file:///path/to/.m2/repository/org/clojure/tools.cli/1.0.206/tools.cli-1.0.206.jar!/clojure/tools/cli.cljc"}}}

  (-> @db/db*
      :documents
      (select-keys ["file:///path/to/code/clojure-lsp/cli/src/clojure_lsp/main.clj"
                    "jar:file:///path/to/.m2/repository/org/clojure/tools.cli/1.0.206/tools.cli-1.0.206.jar!/clojure/tools/cli.cljc"]))
  '{"file:///path/to/code/clojure-lsp/cli/src/clojure_lsp/main.clj"
    {:filename   "/path/to/code/clojure-lsp/cli/src/clojure_lsp/main.clj"
     :internal?  true
     :langs      #{:clj}
     :namespaces #{clojure-lsp.main}}
    "jar:file:///path/to/.m2/repository/org/clojure/tools.cli/1.0.206/tools.cli-1.0.206.jar!/clojure/tools/cli.cljc"
    {:filename   "/path/to/.m2/repository/org/clojure/tools.cli/1.0.206/tools.cli-1.0.206.jar:clojure/tools/cli.cljc"
     :internal?  false
     :langs      #{:clj :cljs}
     :namespaces #{clojure.tools.cli}}}

  (-> @db/db*
      :file-meta
      (select-keys ["/path/to/code/clojure-lsp/cli/src/clojure_lsp/main.clj"
                    "/path/to/.m2/repository/org/clojure/tools.cli/1.0.206/tools.cli-1.0.206.jar:clojure/tools/cli.cljc"]))
  {"/path/to/code/clojure-lsp/cli/src/clojure_lsp/main.clj"
   {:uri "file:///path/to/code/clojure-lsp/cli/src/clojure_lsp/main.clj"}
   "/path/to/.m2/repository/org/clojure/tools.cli/1.0.206/tools.cli-1.0.206.jar:clojure/tools/cli.cljc"
   {:uri "jar:file:///path/to/.m2/repository/org/clojure/tools.cli/1.0.206/tools.cli-1.0.206.jar!/clojure/tools/cli.cljc"}})

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

(defn filename-to-uri [db filename]
  (get-in db [:file-meta filename :uri]))

(defn uri-to-filename [db uri]
  (get-in db [:documents uri :filename]))

(defn uri-internal? [db uri]
  (get-in db [:documents uri :internal?]))

(defn file-internal? [db filename]
  (uri-internal? db (filename-to-uri db filename)))

(defn ^:private update-usage [db f {:keys [from name alias filename lang]}]
  (let [doc (get-in db [:documents (filename-to-uri db filename)])
        usage-langs (or (some-> lang list set) (:langs doc))]
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
        ;; NOTE: We could store :dependents-uris, and look up whether any of
        ;; them are internal. But this way keeps that lookup out of
        ;; q/ns-aliases, which is on the hotpath in completion.
        ;; TODO: is it wrong that once dependents-internal?, always
        ;; dependents-internal? We could stop using a namespace. I think the
        ;; only consequence is that it would continue to show up in completions
        ;; and alias suggestions when it might not have otherwise.
        (update-in [:dep-graph name :dependents-internal?] #(or % (:internal? doc)))
        ;; NOTE: :dependents-langs is used only in add-missing-libspec, so it's
        ;; not on a hotpath, but :dependents-internal? has already established
        ;; this pattern.
        (update-in [:dep-graph name :dependents-langs] #(reduce f % usage-langs)))))

(defn ^:private update-usages [db f namespace-usages]
  (reduce #(update-usage %1 f %2) db namespace-usages))

(defn ^:private remove-usages [db usages] (update-usages db ms-disj usages))
(defn ^:private add-usages    [db usages] (update-usages db ms-conj usages))

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

(defn ^:private update-definition [db s-f ms-f {:keys [name filename]}]
  (let [uri (filename-to-uri db filename)
        in-doc (get-in db [:documents uri])
        doc-langs (:langs in-doc)
        db (-> db
               (update-in [:dep-graph name :uris] s-f uri)
               (update-in [:dep-graph name :internal?] #(or % (:internal? in-doc)))
               (update-in [:documents uri :namespaces] s-f name))]
    (cond-> db
      (contains? doc-langs :clj)
      (update-usage ms-f (assoc clojure-core-ns-usage
                                :from name
                                :filename filename))
      (contains? doc-langs :cljs)
      (update-usage ms-f (assoc cljs-core-ns-usage
                                :from name
                                :filename filename)))))

(defn ^:private update-definitions [db s-f ms-f namespace-definitions]
  (reduce #(update-definition %1 s-f ms-f %2) db namespace-definitions))

(defn ^:private remove-definitions [db namespace-definitions] (update-definitions db s-disj ms-disj namespace-definitions))
(defn ^:private add-definitions    [db namespace-definitions] (update-definitions db s-conj ms-conj namespace-definitions))

(defn ^:private ensure-file [db filename internal?]
  (let [uri (shared/filename->uri filename db)]
    (-> db
        ;; Bridge from filename to URI until we use URI consistently
        (update-in [:file-meta filename] assoc :uri uri)
        ;; TODO: consider using documents as a cache for other things:
        ;; file-type, etc.
        (update-in [:documents uri] assoc
                   :filename filename
                   :internal? internal?
                   :langs (shared/uri->available-langs uri)))))

(defn ^:private ensure-files [db filenames internal?]
  (reduce #(ensure-file %1 %2 internal?) db filenames))

(defn ^:private ns-definitions-and-usages [analysis]
  (let [{:keys [defs usages]}
        (reduce-kv (fn [result filename {:keys [var-definitions namespace-definitions namespace-usages]}]
                     (let [defs (cond-> namespace-definitions
                                  ;; implicitly in user ns
                                  (some #(= 'user (:ns %)) var-definitions)
                                  (conj (assoc user-ns-def :filename filename)))]
                       (-> result
                           (update :defs into defs)
                           (update :usages into namespace-usages))))
                   {:defs [] :usages []}
                   analysis)]
    [defs usages]))

(defn refresh-analysis [db old-analysis new-analysis internal?]
  ;; NOTE: When called during startup this takes a little time (500ms in medium
  ;; projects 1200ms in v. large). Although while in beta not every user will
  ;; need it, we calculate it anyway. This way it can be toggled in the settings
  ;; live, which is useful for beta testing.
  (shared/logging-task
    :maintain-dep-graph
    (let [[old-definitions old-usages] (ns-definitions-and-usages old-analysis)
          [new-definitions new-usages] (ns-definitions-and-usages new-analysis)]
      (-> db
          ;; ensure-files needs to be first
          (ensure-files (keys new-analysis) internal?)
          ;; The rest doesn't depend on ordering, but nicer to define before using
          (remove-usages old-usages)
          (remove-definitions old-definitions)
          (add-definitions new-definitions)
          (add-usages new-usages)))))

(defn remove-file [db uri filename]
  (-> db
      (refresh-analysis (select-keys (:analysis db) [filename])
                        {}
                        (uri-internal? db uri))
      (update :file-meta dissoc filename)))

;;;; File filtering

(def internal-xf ;; works for dep-graph or documents
  (filter (comp :internal? val)))

(def some-dependents-internal-xf ;; works for dep-graph only
  (filter (comp :dependents-internal? val)))

(defn ns-uris [{:keys [dep-graph]} namespace]
  (get-in dep-graph [namespace :uris]))

(defn ns-internal-uris [db namespace]
  (filter #(uri-internal? db %) (ns-uris db namespace)))

(defn ns-dependents-uris [{:keys [dep-graph] :as db} namespace]
  (let [dependents (get-in dep-graph [namespace :dependents])]
    (into #{}
          (mapcat #(ns-uris db %))
          (ms-distinct dependents))))

(defn ns-dependencies-uris [{:keys [dep-graph] :as db} namespace]
  (let [dependencies (get-in dep-graph [namespace :dependencies])]
    (into #{}
          (mapcat #(ns-uris db %))
          (ms-distinct dependencies))))

(defn ns-and-dependents-uris [db namespace]
  (set/union (ns-uris db namespace)
             (ns-dependents-uris db namespace)))

(defn nses-uris [db namespaces]
  (apply set/union
         (map #(ns-uris db %)
              namespaces)))

(defn nses-and-dependents-uris [db namespaces]
  (apply set/union
         (map #(ns-and-dependents-uris db %)
              namespaces)))

(defn internal-uris [{:keys [documents]}]
  (into []
        (comp internal-xf (map key))
        documents))

(ns clojure-lsp.feature.diagnostics.built-in
  (:require
   [clj-kondo.core :as kondo]
   [clojure-lsp.parser :as parser]
   [clojure-lsp.queries :as q]
   [clojure-lsp.refactor.edit :as edit]
   [clojure-lsp.settings :as settings]
   [clojure-lsp.shared :as shared :refer [fast=]]
   [clojure.set :as set]
   [clojure.string :as string]
   [rewrite-clj.zip :as z])
  (:import
   [clojure.lang PersistentVector]))

(set! *warn-on-reflection* true)

(defn ^:private ignore-diag? [diagnostic ignores]
  (let [diag-range {:name-row (-> diagnostic :range :start :line inc)
                    :name-col (-> diagnostic :range :start :character inc)
                    :name-end-row (-> diagnostic :range :end :line inc)
                    :name-end-col (-> diagnostic :range :end :character inc)}
        uri-ignores (get ignores (:uri diagnostic))]
    (some
      (fn [ignore]
        (and
          (or (:all-codes ignore)
              (some #(identical? (keyword (:code diagnostic)) %) (:codes ignore)))
          (shared/inside?
            diag-range
            {:name-row (:row ignore)
             :name-col (:col ignore)
             :name-end-row (:end-row ignore)
             :name-end-col (:end-col ignore)})))
      uri-ignores)))

(def ^:private ignores-keywords #{:clj-kondo/ignore :clojure-lsp/ignore})

(defn ^:private find-ignore-comments [uris db]
  ;; TODO get document text more reliable via file_management/force-get-document
  (let [text-by-uri (reduce #(assoc %1 %2 (or (get-in db [:documents %2 :text])
                                              (shared/slurp-uri %2)
                                              "")) {} uris)
        uris-with-ignores (filterv #(re-find #":clj-kondo/ignore|:clojure-lsp/ignore" (text-by-uri %)) uris)]
    (reduce
      (fn [acc uri]
        (let [ignores (loop [zloc (z/next (parser/safe-zloc-of-string (text-by-uri uri)))
                             ignores []]
                        (if-let [node (z/find-next-tag zloc z/right :uneval)]
                          (cond
                            (some-> node z/next z/next z/sexpr-able? not)
                            (recur node ignores)

                            (and (contains? ignores-keywords (some-> node z/next z/next z/sexpr))
                                 (z/sexpr-able? (z/right node)))
                            (recur node (conj ignores (assoc (meta (z/node (z/right node)))
                                                             :codes (z/sexpr (z/find-next-tag node z/next :vector)))))

                            (contains? ignores-keywords (some-> node z/next z/sexpr))
                            (recur node (conj ignores (assoc (meta (z/node (z/right node)))
                                                             :all-codes true)))

                            :else
                            (recur node ignores))
                          ignores))]
          (assoc acc uri ignores)))
      {}
      uris-with-ignores)))

(defn ^:private element->diagnostic [element level code message tags]
  (shared/assoc-some
    {:uri (:uri element)
     :range {:start {:line (dec (:name-row element))
                     :character (dec (:name-col element))}
             :end {:line (dec (:name-end-row element))
                   :character (dec (:name-end-col element))}}
     :severity (shared/level->severity level)
     :message message
     :code code
     :source "clojure-lsp"}
    :tags tags))

(defn ^:private different-aliases [narrowed-db project-db settings]
  (let [level (get-in settings [:linters :clojure-lsp/different-aliases :level] :off)]
    (when-not (identical? :off level)
      (let [exclude-aliases-config (set (settings/get project-db [:linters :clojure-lsp/different-aliases :exclude-aliases]))
            exclude-aliases (conj exclude-aliases-config nil) ;; nil here means an unaliased require
            dep-graph (:dep-graph narrowed-db)
            inconsistencies (reduce (fn [m [ns dep-graph-item]]
                                      (let [aliases-assigned (-> dep-graph-item :aliases-breakdown :internal keys set (set/difference exclude-aliases))]
                                        (if (> (count aliases-assigned) 1)
                                          (assoc m ns aliases-assigned)
                                          m)))
                                    {}
                                    dep-graph)]
        (for [{dependents :dependents} (map dep-graph (keys inconsistencies))
              [namespace _] dependents
              :let [dep-graph-item (get dep-graph namespace)]
              uri (:uris dep-graph-item)
              :let [var-definition (-> project-db :analysis (get uri))]
              namespace-alias (:namespace-alias var-definition)
              :when (let [{:keys [alias to]} namespace-alias]
                      (and (contains? inconsistencies to)
                           (some #{alias} (get inconsistencies to))))]
          (element->diagnostic
            namespace-alias
            level
            "clojure-lsp/different-aliases"
            (format "Different aliases %s found for %s"
                    (get inconsistencies (:to namespace-alias))
                    (:to namespace-alias))
            nil))))))

(defn ^:private kondo-ns-groups
  "Optimized version of ns-groups that uses memoized variation of re-find with
  signature (string, string) -> boolean"
  [{:keys [re-find-memo ns-groups]} ns-name filename]
  (keep (fn [{:keys [pattern
                     filename-pattern
                     name]}]
          (when (or (and (string? pattern) (symbol? name)
                         (re-find-memo pattern (str ns-name)))
                    (and (string? filename-pattern) (symbol? name)
                         (re-find-memo filename-pattern filename)))
            name))
        ns-groups))

(defn ^:private setting-for-ns [settings definition]
  (let [ns-name (:ns definition)
        filename (-> definition :uri shared/uri->filename)
        ns-groups (cons ns-name (kondo-ns-groups settings ns-name filename))
        configs-in-ns (seq (keep #(get (:config-in-ns settings) %) ns-groups))]
    (reduce
      kondo/merge-configs
      settings
      configs-in-ns)))

(defn ^:private exclude-public-diagnostic-definition? [db settings definition]
  (let [settings (setting-for-ns settings definition)
        excluded-syms-regex (get-in settings [:linters :clojure-lsp/unused-public-var :exclude-regex] #{})
        excluded-defined-by-syms-regex (get-in settings [:linters :clojure-lsp/unused-public-var :exclude-when-defined-by-regex] #{})
        excluded-metas (get-in settings [:linters :clojure-lsp/unused-public-var :exclude-when-contains-meta] #{})
        fqsn (symbol (-> definition :ns str) (-> definition :name str))
        starts-with-dash? (string/starts-with? (:name definition) "-")
        inside-comment? (some #(and (fast= 'comment (:name %))
                                    (fast= 'clojure.core (:ns %))) (:callstack definition))]
    (or inside-comment?
        (q/exclude-public-definition? settings definition)
        (some #(re-matches (re-pattern (str %)) (str fqsn)) excluded-syms-regex)
        (some (fn [exclude]
                (some #(re-matches (re-pattern (str exclude))
                                   (str %))
                      (q/defined-bys definition)))
              excluded-defined-by-syms-regex)
        (some #(-> definition :meta % boolean) excluded-metas)
        (:export definition)
        (when starts-with-dash?
          ;; check if if namespace has :gen-class
          (some-> (parser/zloc-of-file db (:uri definition))
                  edit/find-namespace
                  (z/find-next-value z/next :gen-class))))))

(defn ^:private orphans
  "Given a vector of var-definitions and a map of nses and their dependents,
   returns the var-definitions that are not used in any of the dependents."
  [var-definitions nses-and-dependents]
  (let [var-nses (set (map :ns var-definitions)) ;; optimization to limit usages to internal namespaces, or in the case of a single file, to its namespaces
        usages (into #{}
                     (comp
                       (q/xf-all-var-usages-and-symbols-to-namespaces var-nses)
                       (map (fn [{:keys [bucket] :as e}]
                              (if (identical? :symbols bucket)
                                (q/symbol-signature e)
                                (q/var-usage-signature e)))))
                     nses-and-dependents)
        var-used? (fn [var-def]
                    (some usages (q/var-definition-signatures var-def)))]
    (remove var-used? var-definitions)))

(defn ^:private unused-public-vars [narrowed-db project-db settings]
  (when-not (identical? :off (get-in settings [:linters :clojure-lsp/unused-public-var :level] :info))
    (let [;; cache expensive regex creationg + matching by (string,string) -> boolean
          settings (assoc settings :re-find-memo
                          (let [re-pattern-memo (memoize re-pattern)]
                            (memoize (fn [pattern-str file-str]
                                       (re-find (re-pattern-memo pattern-str) file-str)))))
          ignore-test-references? (get-in settings
                                          [:linters :clojure-lsp/unused-public-var :ignore-test-references?]
                                          false)
          test-locations-regex (into #{}
                                     (map re-pattern
                                          (get settings :test-locations-regex shared/test-locations-regex-default)))
          exclude-def? (partial exclude-public-diagnostic-definition? project-db settings)
          var-definitions (->> (q/find-all-var-definitions narrowed-db false)
                               (remove exclude-def?))
          test-uri? (fn [{uri :uri}] (some #(re-find % uri) test-locations-regex))
          var-definitions-src (remove test-uri? var-definitions)
          var-definitions-test (filter test-uri? var-definitions)
          var-nses (set (map :ns var-definitions)) ;; optimization to limit usages to internal namespaces, or in the case of a single file, to its namespaces
          nses-and-dependents (merge (q/nses-and-dependents-analysis project-db var-nses)
                                     (q/edn-analysis project-db))
          nses-and-dependents-src (into {}
                                        (remove (fn [[uri _]]
                                                  (some #(re-find % uri) test-locations-regex))
                                                nses-and-dependents))
          unused-vars (if ignore-test-references?
                        (concat (orphans var-definitions-src
                                         nses-and-dependents-src)
                                (orphans var-definitions-test
                                         nses-and-dependents))
                        (orphans var-definitions
                                 nses-and-dependents))
          kw-definitions (->> (q/find-all-keyword-definitions narrowed-db)
                              (remove exclude-def?))
          kw-usages (if (seq kw-definitions) ;; avoid looking up thousands of keyword usages if these files don't define any keywords
                      (into #{}
                            (comp
                              q/xf-analysis->keyword-usages
                              (map q/kw-signature))
                            (:analysis project-db))
                      #{})
          kw-used? (fn [kw-def]
                     (contains? kw-usages (q/kw-signature kw-def)))]
      (->> (concat unused-vars
                   (remove kw-used? kw-definitions))
           (keep (fn [element]
                   (let [keyword-def? (identical? :keyword-definitions (:bucket element))
                         settings (if (:ns element)
                                    (setting-for-ns settings element)
                                    settings)
                         level (get-in settings [:linters :clojure-lsp/unused-public-var :level] :info)]
                     (when-not (identical? :off level)
                       (element->diagnostic
                         element
                         level
                         "clojure-lsp/unused-public-var"
                         (if keyword-def?
                           (if (:ns element)
                             (format "Unused public keyword ':%s/%s'" (:ns element) (:name element))
                             (format "Unused public keyword ':%s'" (:name element)))
                           (format "Unused public var '%s/%s'" (:ns element) (:name element)))
                         [1])))))))))

(defn ^:private find-dependency-cycles
  "Detects cycles in the namespace dependency graph using DFS with colors.
   Returns a sequence of cycle paths, where each path is a vector of namespaces forming a cycle."
  [dep-graph]
  (let [;; Get all internal namespaces (ignore external dependencies)
        internal-namespaces (set (keep (fn [[ns data]]
                                         (when (:internal? data) ns))
                                       dep-graph))
        ;; Track node colors: :white (unvisited), :gray (in progress), :black (finished)
        colors (atom (zipmap internal-namespaces (repeat :white)))
        current-path (atom [])
        cycles (atom #{})]

    (letfn [(get-dependencies [namespace]
              ;; Get direct dependencies of a namespace, filtered to internal ones only
              (->> (get-in dep-graph [namespace :dependencies])
                   keys
                   (filter internal-namespaces)))

            (dfs-visit [namespace]
              ;; Depth-first search to detect cycles
              (swap! colors assoc namespace :gray)
              (swap! current-path conj namespace)

              (doseq [dependency (get-dependencies namespace)]
                (case (get @colors dependency)
                  :white (dfs-visit dependency)  ; Unvisited - recurse
                  :gray  ; Back edge found - cycle detected!
                  (let [path ^PersistentVector @current-path
                        cycle-start (.indexOf path dependency)
                        cycle-path (subvec path cycle-start)
                        ;; Create complete cycle by adding the back edge
                        complete-cycle (conj cycle-path dependency)]
                    (swap! cycles conj complete-cycle))
                  :black nil)) ; Already processed - no cycle

              (swap! current-path pop)
              (swap! colors assoc namespace :black))]

      ;; Visit all unvisited namespaces
      (doseq [namespace internal-namespaces]
        (when (= :white (get @colors namespace))
          (dfs-visit namespace)))
      @cycles)))

(defn ^:private cyclic-dependencies
  "Detects cyclic dependencies and generates diagnostics for each namespace in a cycle."
  [narrowed-db _project-db settings]
  (let [level (get-in settings [:linters :clojure-lsp/cyclic-dependencies :level] :off)]
    (when-not (identical? :off level)
      (let [cycles (find-dependency-cycles (:dep-graph narrowed-db))
            exclude-namespaces (set (get-in settings [:linters :clojure-lsp/cyclic-dependencies :exclude-namespaces] #{}))
            ;; Create diagnostics for each namespace in each cycle
            cycle-diagnostics (for [cycle cycles
                                    :let [cycle-path (string/join " -> " cycle)]
                                    namespace (butlast cycle) ;; All namespaces except the duplicate last one
                                    :when (not (contains? exclude-namespaces namespace))
                                    :let [dep-graph-item (get-in narrowed-db [:dep-graph namespace])]
                                    uri (:uris dep-graph-item)
                                    :let [namespace-def (some->> (q/find-namespace-definitions narrowed-db uri)
                                                                 (filter #(= namespace (:name %)))
                                                                 first)]
                                    :when namespace-def]
                                (element->diagnostic
                                  namespace-def
                                  level
                                  "clojure-lsp/cyclic-dependencies"
                                  (format "Cyclic dependency detected: %s" cycle-path)
                                  nil))]
        (distinct cycle-diagnostics)))))

(defn analyze-uris! [uris db]
  (shared/logging-task
    :internal/built-in-linters
    (let [settings (settings/all db)
          project-db (q/db-with-internal-analysis db)
          db-of-uris (update project-db :analysis select-keys uris)
          empty-diags (reduce #(assoc %1 %2 []) {} uris)
          ignores* (future (shared/logging-task
                             :internal/built-in-linters.find-linter-ignore-comments
                             (find-ignore-comments uris db)))
          unused* (future (shared/logging-task
                            :internal/built-in-linters.unused-public-vars
                            (unused-public-vars db-of-uris project-db settings)))
          different* (future (shared/logging-task
                               :internal/built-in-linters.different-aliases
                               (different-aliases db-of-uris project-db settings)))
          cyclic* (future (shared/logging-task
                            :internal/built-in-linters.cyclic-dependencies
                            (cyclic-dependencies db-of-uris project-db settings)))
          all-diags (remove #(ignore-diag? % @ignores*)
                            (concat @unused* @different* @cyclic*))]
      (merge empty-diags
             (reduce
               (fn [acc diag]
                 (update acc (:uri diag) (fnil conj []) (dissoc diag :uri)))
               {}
               all-diags)))))

(defn analyze-uri! [uri db]
  (analyze-uris! [uri] db))

(defn analyze-paths! [paths db]
  (let [uris (mapv #(shared/filename->uri % db) paths)]
    (analyze-uris! (shared/dir-uris->file-uris uris db) db)))

(defn db-with-results [db analyze-fn]
  (update-in db [:diagnostics :built-in] merge (analyze-fn db)))

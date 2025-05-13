(ns clojure-lsp.feature.diagnostics.built-in
  (:require
   [clj-kondo.impl.config :as kondo.config]
   [clojure-lsp.parser :as parser]
   [clojure-lsp.queries :as q]
   [clojure-lsp.refactor.edit :as edit]
   [clojure-lsp.settings :as settings]
   [clojure-lsp.shared :as shared]
   [clojure.set :as set]
   [clojure.string :as string]
   [rewrite-clj.zip :as z]))

(defn ^:private element->diagnostic [element severity code message]
  {:uri (:uri element)
   :range {:start {:line (dec (:name-row element))
                   :character (dec (:name-col element))}
           :end {:line (dec (:name-end-row element))
                 :character (dec (:name-end-col element))}}
   :severity (case severity
               :error 1
               :warning 2
               :info 3)
   :message message
   :code code
   :source "clojure-lsp"})

(defn ^:private different-aliases [narrowed-db project-db kondo-config]
  (let [kondo-config (update-in kondo-config [:linters :clojure-lsp/different-aliases :level] #(or % :off))]
    (when-not (identical? :off (-> kondo-config :linters :clojure-lsp/different-aliases :level))
      (let [exclude-aliases-config (-> kondo-config :linters :clojure-lsp/different-aliases :exclude-aliases set)
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
            (-> kondo-config :linters :clojure-lsp/different-aliases :level)
            "clojure-lsp/different-aliases"
            (format "Different aliases %s found for %s"
                    (get inconsistencies (:to namespace-alias))
                    (:to namespace-alias))))))))

(defn ^:private kondo-config-for-ns [kondo-config ns-name filename]
  (let [ns-groups (cons ns-name (kondo.config/ns-groups kondo-config ns-name filename))
        configs-in-ns (seq (keep #(get (:config-in-ns kondo-config) %) ns-groups))
        kondo-config (if configs-in-ns
                       (apply kondo.config/merge-config! kondo-config configs-in-ns)
                       kondo-config)]
    kondo-config))

(defn ^:private exclude-public-diagnostic-definition? [db kondo-config definition]
  (let [kondo-config (kondo-config-for-ns kondo-config (:ns definition) (-> definition :uri shared/uri->filename))
        excluded-syms-regex (get-in kondo-config [:linters :clojure-lsp/unused-public-var :exclude-regex] #{})
        excluded-defined-by-syms-regex (get-in kondo-config [:linters :clojure-lsp/unused-public-var :exclude-when-defined-by-regex] #{})
        excluded-metas (get-in kondo-config [:linters :clojure-lsp/unused-public-var :exclude-when-contains-meta] #{})
        fqsn (symbol (-> definition :ns str) (-> definition :name str))
        starts-with-dash? (string/starts-with? (:name definition) "-")
        inside-comment? (some #(and (= 'comment (:name %))
                                    (= 'clojure.core (:ns %))) (:callstack definition))]
    (or inside-comment?
        (q/exclude-public-definition? kondo-config definition)
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

(defn ^:private unused-public-vars [narrowed-db project-db kondo-config]
  (when-not (identical? :off (-> kondo-config :linters :clojure-lsp/unused-public-var :level))
    (let [ignore-test-references? (get-in kondo-config
                                          [:linters :clojure-lsp/unused-public-var :ignore-test-references?]
                                          false)
          test-locations-regex (into #{}
                                     (map re-pattern
                                          (settings/get project-db
                                                        [:test-locations-regex]
                                                        shared/test-locations-regex-default)))
          exclude-def? (partial exclude-public-diagnostic-definition? project-db kondo-config)
          var-definitions (->> (q/find-all-var-definitions narrowed-db)
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
                              q/xf-all-keyword-usages
                              (map q/kw-signature))
                            (:analysis project-db))
                      #{})
          kw-used? (fn [kw-def]
                     (contains? kw-usages (q/kw-signature kw-def)))]
      (->> (concat unused-vars
                   (remove kw-used? kw-definitions))
           (map (fn [element]
                  (let [keyword-def? (identical? :keyword-definitions (:bucket element))
                        kondo-config (if (:ns element)
                                       (kondo-config-for-ns kondo-config (:ns element) (-> element :uri shared/uri->filename))
                                       kondo-config)
                        severity (or (-> kondo-config :linters :clojure-lsp/unused-public-var :level) :info)]
                    (element->diagnostic
                      element
                      severity
                      "clojure-lsp/unused-public-var"
                      (if keyword-def?
                        (if (:ns element)
                          (format "Unused public keyword ':%s/%s'" (:ns element) (:name element))
                          (format "Unused public keyword ':%s'" (:name element)))
                        (format "Unused public var '%s/%s'" (:ns element) (:name element)))))))))))

(defn analyze-uris! [uris db]
  (let [project-db (q/db-with-internal-analysis db)
        db-of-uris (update project-db :analysis select-keys uris)
        all-diags (concat
                    (unused-public-vars db-of-uris project-db (:kondo-config db))
                    (different-aliases db-of-uris project-db (:kondo-config db)))]
    (reduce
      (fn [acc diag]
        (update acc (:uri diag) (fnil conj []) (dissoc diag :uri)))
      {}
      all-diags)))

(defn analyze-uri! [uri db]
  (analyze-uris! [uri] db))

(defn analyze-paths! [paths db]
  (let [uris (mapv #(shared/filename->uri % db) paths)]
    (analyze-uris! (shared/dir-uris->file-uris uris db) db)))

(defn db-with-results [db analyze-fn]
  (update-in db [:diagnostics :built-in] merge (analyze-fn db)))

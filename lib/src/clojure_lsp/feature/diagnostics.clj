(ns clojure-lsp.feature.diagnostics
  (:require
   [cheshire.core :as json]
   [clj-kondo.impl.config :as kondo.config]
   [clojure-lsp.dep-graph :as dep-graph]
   [clojure-lsp.logger :as logger]
   [clojure-lsp.parser :as parser]
   [clojure-lsp.queries :as q]
   [clojure-lsp.refactor.edit :as edit]
   [clojure-lsp.settings :as settings]
   [clojure-lsp.shared :as shared]
   [clojure-lsp.snapshot :as snapshot]
   [clojure.core.async :as async]
   [clojure.set :as set]
   [clojure.string :as string]
   [rewrite-clj.zip :as z])
  (:gen-class))

(set! *warn-on-reflection* true)

(def diagnostic-types-of-unnecessary-type
  #{:clojure-lsp/unused-public-var
    :redefined-var
    :redundant-do
    :redundant-expression
    :redundant-let
    :unused-binding
    :unreachable-code
    :unused-import
    :unused-namespace
    :unused-private-var
    :unused-referred-var})

(def deprecated-diagnostic-types
  #{:deprecated-var})

(defn ^:private kondo-config-for-ns [kondo-config ns-name filename]
  (let [ns-groups (cons ns-name (kondo.config/ns-groups kondo-config ns-name filename))
        configs-in-ns (seq (keep #(get (:config-in-ns kondo-config) %) ns-groups))
        kondo-config (if configs-in-ns
                       (apply kondo.config/merge-config! kondo-config configs-in-ns)
                       kondo-config)]
    kondo-config))

(defn ^:private unused-public-var->finding [element kondo-config]
  (let [keyword-def? (identical? :keyword-definitions (:bucket element))
        kondo-config (if (:ns element)
                       (kondo-config-for-ns kondo-config (:ns element) (-> element :uri shared/uri->filename))
                       kondo-config)]
    {:uri (:uri element)
     :row (:name-row element)
     :col (:name-col element)
     :end-row (:name-end-row element)
     :end-col (:name-end-col element)
     :level (or (-> kondo-config :linters :clojure-lsp/unused-public-var :level) :info)
     :message (if keyword-def?
                (if (:ns element)
                  (format "Unused public keyword ':%s/%s'" (:ns element) (:name element))
                  (format "Unused public keyword ':%s'" (:name element)))
                (format "Unused public var '%s/%s'" (:ns element) (:name element)))
     :type :clojure-lsp/unused-public-var}))

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

(defn ^:private diagnostics-start-char-coll? [lines* row col]
  (let [start-char (when-let [^String line (get @lines* (dec row))]
                     (try (.charAt line (dec col))
                          (catch StringIndexOutOfBoundsException _ nil)))]
    (contains? #{\( \[ \{} start-char)))

(defn ^:private kondo-finding->diagnostic
  [range-type
   lines*
   output-langs?
   {:keys [type message level row col end-row langs refers] :as finding}]
  (let [expression? (not= row end-row)
        simple-range? (and (not (identical? :full range-type))
                           (diagnostics-start-char-coll? lines* row col))
        finding (cond-> (merge {:end-row row :end-col col} finding)
                  (or expression? simple-range?) (assoc :end-row row :end-col col))]
    (shared/assoc-some
      {:range (shared/->range finding)
       :tags (cond-> []
               (diagnostic-types-of-unnecessary-type type) (conj 1)
               (deprecated-diagnostic-types type) (conj 2))
       :message (str message
                     (when (and output-langs?
                                (seq langs))
                       (str " [" (string/join ", " (map name langs)) "]")))
       :code (if-let [n (namespace type)]
               (str n "/" (name type))
               (name type))
       :langs langs
       :severity (case level
                   :error 1
                   :warning 2
                   :info 3)
       :source (if (identical? :clojure-lsp/unused-public-var type)
                 "clojure-lsp"
                 "clj-kondo")}
      :data (when refers
              {:refers refers}))))

(defn ^:private valid-finding? [{:keys [row col level] :as finding}]
  (when (not= level :off)
    (or (and row col)
        (logger/warn "Invalid clj-kondo finding. Cannot find position data for" finding))))

(defn ^:private exclude-ns? [uri linter db]
  ;; TODO: it's possible, though unusual, to have several namespaces in a file.
  ;; What should we do in that case?
  (when-let [namespace (first (dep-graph/ns-names-for-uri db uri))]
    (when-let [ns-exclude-regex-str (settings/get db [:linters linter :ns-exclude-regex])]
      (re-matches (re-pattern ns-exclude-regex-str) (str namespace)))))

(defn ^:private kondo-findings->diagnostics [uri linter db]
  (let [range-type (settings/get db [:diagnostics :range-type] :full)
        ;; we delay for performance
        lines* (delay (some-> (get-in db [:documents uri :text])
                              (string/split #"\r?\n")))
        output-langs? (some-> db :kondo-config :output :langs)]
    (when-not (exclude-ns? uri linter db)
      (->> (get-in db [:findings uri])
           (filter valid-finding?)
           (mapv #(kondo-finding->diagnostic range-type lines* output-langs? %))))))

(defn severity->level [severity]
  (case (int severity)
    1 :error
    2 :warning
    3 :info))

(defn severity->color [severity]
  (case (int severity)
    1 :red
    2 :yellow
    3 :cyan))

(defn ^:private clj-depend-violations->diagnostics [uri level db]
  ;; TODO: it's possible, though unusual, to have several namespaces in a file.
  ;; What should we do in that case?
  (when-let [namespace (first (dep-graph/ns-names-for-uri db uri))]
    (mapv (fn [{:keys [message]}]
            (let [ns-definition (q/find-namespace-definition-by-uri db uri)]
              {:range (shared/->range ns-definition)
               :tags []
               :message message
               :code "clj-depend"
               :severity (case level
                           :error 1
                           :warning 2
                           :info 3)
               :source "clj-depend"}))
          (get-in db [:clj-depend-violations (symbol namespace)]))))

(defn find-diagnostics* [^String uri db]
  (let [kondo-level (settings/get db [:linters :clj-kondo :level])
        depend-level (settings/get db [:linters :clj-depend :level] :info)]
    (if (shared/jar-file? uri)
      []
      (cond-> []
        (not= :off kondo-level)
        (concat (kondo-findings->diagnostics uri :clj-kondo db))

        (not= :off depend-level)
        (concat (clj-depend-violations->diagnostics uri depend-level db))))))

(defn diagnostic->diagnostic-message [file-output raw? {:keys [message severity range code]}]
  (cond-> (format "%s:%s:%s: %s: [%s] %s"
                  file-output
                  (-> range :start :line inc)
                  (-> range :start :character inc)
                  (name (severity->level severity))
                  code
                  message)
    (not raw?) (shared/colorize (severity->color severity))))

(def ^:dynamic *discard-duration* (atom []))

(defn discarding-duration-ms []
  (let [durations @*discard-duration*]
    (reset! *discard-duration* [])
    (float (/ (apply + durations) 1000000))))

(defn find-diagnostics [^String uri db]
  (let [diagnostics (find-diagnostics* uri db)]
    (if @snapshot/cache
      (let [start (System/nanoTime)
            project-path (shared/uri->filename (shared/project-root->uri nil db))
            filename (shared/uri->filename uri)
            relative-path (shared/relativize-filepath filename project-path)
            messages (snapshot/discard relative-path)
            result (if messages
                     (into []
                           (remove #(contains? messages
                                               (diagnostic->diagnostic-message relative-path true %))
                                   diagnostics))
                     diagnostics)]
        (swap! *discard-duration* conj (- (System/nanoTime) start))
        result)
      diagnostics)))

(defn ^:private publish-diagnostic!* [{:keys [diagnostics-chan]} diagnostic]
  (async/put! diagnostics-chan diagnostic))

(defn ^:private publish-all-diagnostics!* [{:keys [diagnostics-chan]} diagnostics]
  (async/onto-chan! diagnostics-chan diagnostics false))

(defn ^:private diagnostics-of-uri [uri db]
  {:uri uri
   :diagnostics (find-diagnostics uri db)})

(defn ^:private empty-diagnostics-of-uri [uri]
  {:uri uri
   :diagnostics []})

(defn publish-diagnostics! [uri {:keys [db*], :as components}]
  (publish-diagnostic!* components (diagnostics-of-uri uri @db*)))

(defn publish-all-diagnostics! [uris {:keys [db*], :as components}]
  (let [db @db*]
    (publish-all-diagnostics!*
      components
      (->> uris
           (remove #(= :unknown (shared/uri->file-type %)))
           (map #(diagnostics-of-uri % db))))))

(defn publish-empty-diagnostics! [uris components]
  (publish-all-diagnostics!* components (map empty-diagnostics-of-uri uris)))

(defn ^:private namespace-alias->finding [element inconsistencies kondo-config]
  {:uri (:uri element)
   :row (:name-row element)
   :col (:name-col element)
   :end-row (:name-end-row element)
   :end-col (:name-end-col element)
   :level (-> kondo-config :linters :clojure-lsp/different-aliases :level)
   :message (format "Different aliases %s found for %s"
                    (get inconsistencies (:to element))
                    (:to element))
   :type :clojure-lsp/different-aliases})

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
          (namespace-alias->finding namespace-alias inconsistencies kondo-config))))))

(defn ^:private unused-public-vars [narrowed-db project-db kondo-config]
  (when-not (identical? :off (-> kondo-config :linters :clojure-lsp/unused-public-var :level))
    (let [exclude-def? (partial exclude-public-diagnostic-definition? project-db kondo-config)
          var-definitions (->> (q/find-all-var-definitions narrowed-db)
                               (remove exclude-def?))
          var-nses (set (map :ns var-definitions)) ;; optimization to limit usages to internal namespaces, or in the case of a single file, to its namespaces
          var-usages (into #{}
                           (comp
                             (q/xf-all-var-usages-to-namespaces var-nses)
                             (map q/var-usage-signature))
                           (q/nses-and-dependents-analysis project-db var-nses))
          var-used? (fn [var-def]
                      (some var-usages (q/var-definition-signatures var-def)))
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
      (->> (concat (remove var-used? var-definitions)
                   (remove kw-used? kw-definitions))
           (map (fn [unused-var]
                  (unused-public-var->finding unused-var kondo-config)))))))

(defn ^:private findings-of-project
  [db kondo-config]
  (let [project-db (q/db-with-internal-analysis db)]
    (concat (unused-public-vars project-db project-db kondo-config)
            (different-aliases project-db project-db kondo-config))))

(defn ^:private findings-of-uris
  [uris db kondo-config]
  (let [project-db (q/db-with-internal-analysis db)
        db-of-uris (update project-db :analysis select-keys uris)]
    (concat (unused-public-vars db-of-uris project-db kondo-config)
            (different-aliases db-of-uris project-db kondo-config))))

(defn ^:private finalize-findings! [findings reg-finding!]
  (let [uri->filename (memoize shared/uri->filename)]
    (run! #(reg-finding! (assoc % :filename (uri->filename (:uri %)))) findings))
  nil)

(defn custom-lint-project!
  [db {:keys [reg-finding! config]}]
  (-> (findings-of-project db config)
      (finalize-findings! reg-finding!)))

(defn custom-lint-uris!
  [uris db {:keys [reg-finding! config]}]
  (-> (findings-of-uris uris db config)
      (finalize-findings! reg-finding!)))

(defn diagnostics->diagnostic-messages [diagnostics {:keys [project-root output raw?]} db]
  (let [project-path (shared/uri->filename (shared/project-root->uri project-root db))]
    (mapcat (fn [[uri diags]]
              (let [filename (shared/uri->filename uri)
                    file-output (if (:canonical-paths output)
                                  filename
                                  (shared/relativize-filepath filename project-path))]
                (map #(diagnostic->diagnostic-message file-output raw? %)
                     diags)))
            diagnostics)))

(defn serialize [format diags-by-uri options db]
  (case format
    :edn (with-out-str (pr diags-by-uri))
    :json (json/generate-string diags-by-uri)
    (string/join "\n" (diagnostics->diagnostic-messages diags-by-uri options db))))

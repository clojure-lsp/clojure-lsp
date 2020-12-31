(ns clojure-lsp.feature.completion
  (:require
    [clojure-lsp.clojure-core :as cc]
    [clojure-lsp.feature.documentation :as f.documentation]
    [clojure-lsp.refactor.transform :as r.transform]
    [clojure-lsp.shared :as shared]
    [clojure.set :as set]
    [clojure.string :as string]))

(defn ^:private matches-cursor? [cursor-value s]
  (when (and s (string/starts-with? s cursor-value))
    s))

(defn completion [uri line column file-envs remote-envs cursor-loc cursor-usage]
  (let [local-env (get file-envs uri)
        {cursor-value :str cursor-file-type :file-type} cursor-usage
        [cursor-ns _] (if-let [idx (some-> cursor-value (string/index-of "/"))]
                        [(subs cursor-value 0 idx) (subs cursor-value (inc idx))]
                        [cursor-value nil])
        matches? (partial matches-cursor? cursor-value)
        namespaces-and-aliases (->> file-envs
                                    (mapcat val)
                                    (filter (fn [{:keys [file-type tags] :as _usage}]
                                              (and
                                                (= cursor-file-type file-type)
                                                (or
                                                  (set/subset? #{:public :ns} tags)
                                                  (:alias tags)))))
                                    (mapv (fn [{:keys [sym] alias-str :str alias-ns :ns :as _usage}]
                                            [alias-str {:label (name sym)
                                                        :detail (if alias-ns
                                                                  (str alias-ns)
                                                                  (name sym))
                                                        :alias-str alias-str
                                                        :alias-ns alias-ns}]))
                                    (distinct)
                                    (reduce (fn [m [k v]]
                                              (update m k (fnil conj []) v))
                                            {}))
        remotes-by-ns (->> (for [[_ usages] remote-envs
                                 usage usages
                                 :when (and (set/subset? #{:ns :public} (:tags usage))
                                            (= cursor-file-type (:file-type usage)))]
                             [(:sym usage) usages])
                           (into {}))]
    (when cursor-value
      (if (contains? (:tags cursor-usage) :refer)
        ;; If the cursor is within a :refer then the sole
        ;; completions should be symbols within the namespace
        ;; that's being referred
        (->> (get remotes-by-ns (:ns cursor-usage))
             (filter #(every? (:tags %) [:declare :public]))
             (filter (comp matches? :str))
             (map (fn [candidate]
                    {:label (:str candidate)
                     :detail (str (:sym candidate))
                     :data (str (:sym candidate))})))
        ;; Otherwise, completions could come from various sources
        (concat
          (->> local-env
               (filter (comp :declare :tags))
               (filter (comp matches? :str))
               (remove (fn [usage]
                         (when-let [scope-bounds (:scope-bounds usage)]
                           (not= :within (shared/check-bounds line column scope-bounds)))))
               (mapv (fn [{:keys [sym kind]}]
                       (cond-> {:label (name sym)
                                :data (str sym)}
                         kind (assoc :kind kind))))
               (sort-by :label))
          (->> namespaces-and-aliases
               (filter (comp matches? key))
               (mapcat val)
               (mapv (fn [{:keys [alias-str alias-ns] :as info}]
                       (let [require-edit (some-> cursor-loc
                                                  (r.transform/add-known-libspec (symbol alias-str) alias-ns)
                                                  (r.transform/result))]
                         (cond-> (dissoc info :alias-ns :alias-str)
                           require-edit (assoc :additional-text-edits (mapv #(update % :range shared/->range) require-edit))))))
               (sort-by :label))
          (->> (for [[alias-str matches] namespaces-and-aliases
                     :when (= alias-str cursor-ns)
                     {:keys [alias-ns]} matches
                     :let [usages (get remotes-by-ns alias-ns)]
                     usage usages
                     :when (and (get-in usage [:tags :public])
                                (not (get-in usage [:tags :ns]))
                                (= cursor-file-type (:file-type usage)))
                     :let [require-edit (some-> cursor-loc
                                                (r.transform/add-known-libspec (symbol alias-str) alias-ns)
                                                (r.transform/result))]]
                 (cond-> {:label (str alias-str "/" (name (:sym usage)))
                          :detail (name alias-ns)
                          :data (str (:sym usage))}
                   require-edit (assoc :additional-text-edits (mapv #(update % :range shared/->range) require-edit))))
               (sort-by :label))
          (->> cc/core-syms
               (filter (comp matches? str))
               (map (fn [sym] {:label (str sym)
                               :data (str "clojure.core/" sym)}))
               (sort-by :label))
          (when (or (contains? cursor-file-type :cljc)
                    (contains? cursor-file-type :cljs))
            (->> cc/cljs-syms
                 (filter (comp matches? str))
                 (map (fn [sym] {:label (str sym)
                                 :data (str "cljs.core/" sym)}))
                 (sort-by :label)))
          (when (or (contains? cursor-file-type :cljc)
                    (contains? cursor-file-type :clj))
            (->> cc/java-lang-syms
                 (filter (comp matches? str))
                 (map (fn [sym] {:label (str sym)
                                 :data (str "java.lang." sym)}))
                 (sort-by :label))))))))

(defn resolve-item [label sym-wanted file-envs client-capabilities settings]
  (let [usage (first
                (for [[_ usages] file-envs
                      {:keys [sym tags] :as usage} usages
                      :when (and (= (str sym) sym-wanted)
                                 (:declare tags))]
                  usage))
        [content-format] (get-in client-capabilities [:text-document :completion :completion-item :documentation-format])
        show-docs-arity-on-same-line? (get-in settings [:show-docs-arity-on-same-line?])]
    {:label label
     :data sym-wanted
     :documentation (f.documentation/generate content-format usage show-docs-arity-on-same-line?)}))

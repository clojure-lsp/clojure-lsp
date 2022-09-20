(ns clojure-lsp.feature.rename
  (:require
   [clojure-lsp.queries :as q]
   [clojure-lsp.settings :as settings]
   [clojure-lsp.shared :as shared]
   [clojure.string :as string]))

(set! *warn-on-reflection* true)

(defn ident-split [ident-str]
  (let [ident-conformed (some-> ident-str (string/replace #"^::?" ""))
        prefix          (string/replace ident-str #"^(::?)?.*" "$1")
        idx             (string/index-of ident-conformed "/")]
    (if (and idx (not= idx (dec (count ident-conformed))))
      (into [prefix] (string/split ident-conformed #"/" 2))
      [prefix nil ident-conformed])))

(defn ^:private rename-keyword
  [replacement
   replacement-raw
   db
   {:keys [ns alias name uri
           name-col name-end-col
           namespace-from-prefix
           keys-destructuring] :as reference}]
  (let [version (get-in db [:documents uri :v] 0)
        ;; Infers if the qualified keyword is of the ::kw-name kind
        ;; So the same-ns style or the full qualified name can be preserved
        ;; The 2 accounts for the 2 colons in same-namespace qualified keyword
        qualified-same-ns? (= (- name-end-col name-col)
                              (+ 2 (count name)))
        ;; Extracts the name of the keyword
        ;; Maybe have the replacement analyzed by clj-kondo instead?
        replacement-name (string/replace replacement #":+(.+/)?" "")
        ;; Extracts the namespace of the keyword
        ;; Maybe have the replacement analyzed by clj-kondo instead?
        replacement-ns (string/replace replacement-raw #":+(.+)/.+" "$1")
        namespace-changed? (and ns
                                replacement-ns
                                ;; allow only simple namespaced keywords, not aliased keywords
                                (re-matches #"^:(.+)/.+" replacement-raw))
        ;; we find the locals analysis since when destructuring we have both
        ;; keyword and a locals analysis for the same position
        local-element (when keys-destructuring
                        (q/find-local-by-destructured-keyword db uri reference))
        text (cond
               (and local-element
                    (string/includes? (:str local-element) "/")
                    (string/starts-with? (:str local-element) ":"))
               (str ":" ns "/" replacement-name)

               (and local-element
                    (string/includes? (:str local-element) "/"))
               (str ns "/" replacement-name)

               local-element
               (str replacement-name)

               alias
               (str "::" alias "/" replacement-name)

               (and qualified-same-ns?
                    ;; check if it is from aliased keyword -> namespaced keyword
                    (string/starts-with? replacement-raw "::"))
               (str "::" replacement-name)

               (and qualified-same-ns?
                    ;; check if is from aliased keyword -> namespaced keyword
                    (string/starts-with? replacement-raw ":"))
               replacement-raw

               namespace-from-prefix
               (str ":" replacement-name)

               namespace-changed?
               (str ":" replacement-ns "/" replacement-name)

               ns
               (str ":" ns "/" replacement-name)

               ;; There shouldn't be another case, since renaming
               ;; unqualified keywords is currently disallowed
               :else
               replacement)]
    (concat
      [{:range (shared/->range reference)
        :new-text text
        :text-document {:version version :uri uri}}]
      (when local-element
        (->> (q/find-references db local-element false)
             (map (fn [reference]
                    {:range (shared/->range reference)
                     :new-text replacement-name
                     :text-document {:version version :uri uri}})))))))

(defn ^:private rename-ns-definition
  [replacement
   db
   reference]
  (let [ref-doc-uri (:uri reference)
        version (get-in db [:documents ref-doc-uri :v] 0)
        text (if (contains? #{:keyword-definitions :keyword-usages} (:bucket reference))
               (str ":" replacement "/" (:name reference))
               replacement)]
    {:range (shared/->range reference)
     :new-text text
     :text-document {:version version :uri ref-doc-uri}}))

(defn ^:private rename-alias [replacement db reference]
  (let [alias? (= :namespace-alias (:bucket reference))
        keyword? (contains? #{:keyword-definitions :keyword-usages} (:bucket reference))
        ref-doc-uri (:uri reference)
        [u-prefix _ u-name] (when-not alias?
                              (ident-split (:name reference)))
        version (get-in db [:documents ref-doc-uri :v] 0)]
    (if keyword?
      {:range (shared/->range reference)
       :new-text (str "::" replacement "/" (:name reference))
       :text-document {:version version :uri ref-doc-uri}}
      {:range (shared/->range reference)
       :new-text (if alias? replacement (str u-prefix replacement "/" u-name))
       :text-document {:version version :uri ref-doc-uri}})))

(defn ^:private rename-local
  [replacement db reference]
  (let [name-start (- (:name-end-col reference) (count (name (:name reference))))
        ref-doc-uri (:uri reference)
        version (get-in db [:documents ref-doc-uri :v] 0)]
    (if (string/starts-with? replacement ":")
      {:range (shared/->range (assoc reference
                                     :name-col name-start))
       :new-text (subs replacement 1)
       :text-document {:version version :uri ref-doc-uri}}
      {:range (shared/->range (assoc reference :name-col name-start))
       :new-text replacement
       :text-document {:version version :uri ref-doc-uri}})))

(defn ^:private rename-other
  [replacement db reference]
  (let [name-start (- (:name-end-col reference) (count (name (:name reference))))
        ref-doc-uri (:uri reference)
        version (get-in db [:documents ref-doc-uri :v] 0)]
    {:range (shared/->range (assoc reference :name-col name-start))
     :new-text replacement
     :text-document {:version version :uri ref-doc-uri}}))

(defn ^:private rename-defrecord
  [replacement db reference]
  (let [current-name (str (:name reference))
        map->? (string/starts-with? current-name "map->")
        ->? (string/starts-with? current-name "->")
        name-end (+ (:name-col reference) (count (name current-name)))
        ref-doc-uri (:uri reference)
        version (get-in db [:documents ref-doc-uri :v] 0)]
    {:new-text (cond
                 map->?
                 (str "map->" replacement)

                 ->?
                 (str "->" replacement)

                 :else
                 replacement)
     :text-document {:version version :uri ref-doc-uri}
     :range (shared/->range (assoc reference :name-end-col name-end))}))

(defn ^:private rename-changes
  [element definition references replacement replacement-raw db]
  (cond
    (identical? :namespace-alias (:bucket element))
    (mapv (partial rename-alias replacement db) references)

    (identical? :namespace-definitions (:bucket definition))
    (mapv (partial rename-ns-definition replacement db) references)

    (contains? #{:keyword-definitions :keyword-usages} (:bucket definition))
    (vec (mapcat (partial rename-keyword replacement replacement-raw db) references))

    (identical? :locals (:bucket definition))
    (mapv (partial rename-local replacement db) references)

    (and (identical? :var-definitions (:bucket definition))
         (contains? '#{clojure.core/defrecord cljs.core/defrecord}
                    (:defined-by definition)))
    (->> references
         (remove #(and (identical? :var-definitions (:bucket %))
                       (or (string/starts-with? (str (:name %)) "->")
                           (string/starts-with? (str (:name %)) "map->"))))
         (mapv (partial rename-defrecord replacement db)))

    :else
    (mapv (partial rename-other replacement db) references)))

(defn ^:private rename-status [db element]
  (let [references (q/find-references db element true)
        definition (q/find-definition db element)
        client-capabilities (:client-capabilities db)
        source-paths (settings/get db [:source-paths])
        source-path (shared/uri->source-path (:uri definition) source-paths)]
    (cond
      (empty? references)
      {:error {:message "Can't rename, no other references found."
               :code :invalid-params}}

      (and (= :namespace-definitions (:bucket definition))
           (not= :namespace-alias (:bucket element))
           (not source-path))
      {:error {:code :invalid-params
               :message "Can't rename namespace, invalid source-paths. Are project :source-paths configured correctly?"}}

      (and (= :namespace-definitions (:bucket definition))
           (not= :namespace-alias (:bucket element))
           (not (get-in client-capabilities [:workspace :workspace-edit :document-changes])))
      {:error {:code :invalid-params
               :message "Can't rename namespace, client does not support file renames."}}

      (and (contains? #{:keyword-definitions :keyword-usages} (:bucket definition))
           (not (:ns definition)))
      {:error {:code :invalid-params
               :message "Can't rename, only namespaced keywords can be renamed."}}

      :else
      {:result :success
       :references references
       :definition definition
       :source-path source-path})))

(def ^:private error-no-element
  {:error {:code :invalid-params
           :message "Can't rename, no element found."}})

(defn prepare-rename
  [uri row col db]
  (let [element (q/find-element-under-cursor db uri row col)]
    (if-not element
      error-no-element
      (let [{:keys [error] :as result} (rename-status db element)]
        (if error
          result
          (shared/->range element))))))

(defn rename-element [uri new-name db element source]
  (let [{:keys [error] :as result} (rename-status db element)]
    (if error
      result
      (let [{:keys [references definition source-path]} result
            replacement (string/replace new-name #".*/([^/]*)$" "$1")
            changes (rename-changes element definition references replacement new-name db)
            doc-changes (->> changes
                             (group-by :text-document)
                             (remove (comp empty? val))
                             (map (fn [[text-document edits]]
                                    {:text-document text-document
                                     :edits (mapv #(dissoc % :text-document) edits)})))]
        (if (and (identical? :namespace-definitions (:bucket definition))
                 (not (identical? :namespace-alias (:bucket element)))
                 (not= :rename-file source))
          (let [def-uri (:uri definition)
                file-type (shared/uri->file-type def-uri)
                new-uri (shared/namespace->uri replacement source-path file-type db)]
            (shared/client-changes (concat doc-changes
                                           [{:kind "rename"
                                             :old-uri uri
                                             :new-uri new-uri}])
                                   db))
          (shared/client-changes doc-changes db))))))

(defn rename-from-position
  [uri new-name row col db]
  (if-let [element (q/find-element-under-cursor db uri row col)]
    (rename-element uri new-name db element :rename)
    error-no-element))

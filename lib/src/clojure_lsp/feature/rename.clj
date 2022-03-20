(ns clojure-lsp.feature.rename
  (:require
   [clojure-lsp.queries :as q]
   [clojure-lsp.settings :as settings]
   [clojure-lsp.shared :as shared]
   [clojure.set :as set]
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
   {:keys [ns alias name filename
           name-col name-end-col
           namespace-from-prefix
           keys-destructuring] :as reference}]
  (let [ref-doc-uri (shared/filename->uri filename db)
        version (get-in @db [:documents ref-doc-uri :v] 0)
        ;; Extracts the name of the keyword
        ;; Maybe have the replacement analyzed by clj-kondo instead?
        replacement-name (string/replace replacement #":+(.+/)?" "")
        ;; Infers if the qualified keyword is of the ::kw-name kind
        ;; So the same-ns style or the full qualified name can be preserved
        ;; The 2 accounts for the 2 colons in same-namespace qualified keyword
        qualified-same-ns? (= (- name-end-col name-col)
                              (+ 2 (count name)))
        ;; we find the locals analysis since when destructuring we have both
        ;; keyword and a locals analysis for the same position
        local-element (when keys-destructuring
                        (q/find-local-by-destructured-keyword (:analysis @db) filename reference))
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

               ns
               (str ":" ns "/" replacement-name)

               ;; There shouldn't be another case, since renaming
               ;; unqualified keywords is currently disallowed
               :else
               replacement)]

    {:range (shared/->range reference)
     :new-text text
     :text-document {:version version :uri ref-doc-uri}}))

(defn ^:private rename-ns-definition
  [replacement
   db
   reference]
  (let [ref-doc-id (shared/filename->uri (:filename reference) db)
        version (get-in @db [:documents ref-doc-id :v] 0)
        text (if (identical? :keywords (:bucket reference))
               (str ":" replacement "/" (:name reference))
               replacement)]
    {:range (shared/->range reference)
     :new-text text
     :text-document {:version version :uri ref-doc-id}}))

(defn ^:private rename-alias [replacement db reference]
  (let [alias? (= :namespace-alias (:bucket reference))
        keyword? (= :keywords (:bucket reference))
        ref-doc-uri (shared/filename->uri (:filename reference) db)
        [u-prefix _ u-name] (when-not alias?
                              (ident-split (:name reference)))
        version (get-in @db [:documents ref-doc-uri :v] 0)]
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
        ref-doc-id (shared/filename->uri (:filename reference) db)
        version (get-in @db [:documents ref-doc-id :v] 0)]
    (if (string/starts-with? replacement ":")
      {:range (shared/->range (assoc reference
                                     :name-col name-start))
       :new-text (subs replacement 1)
       :text-document {:version version :uri ref-doc-id}}
      {:range (shared/->range (assoc reference :name-col name-start))
       :new-text replacement
       :text-document {:version version :uri ref-doc-id}})))

(defn ^:private rename-other
  [replacement db reference]
  (let [name-start (- (:name-end-col reference) (count (name (:name reference))))
        ref-doc-id (shared/filename->uri (:filename reference) db)
        version (get-in @db [:documents ref-doc-id :v] 0)]
    {:range (shared/->range (assoc reference :name-col name-start))
     :new-text replacement
     :text-document {:version version :uri ref-doc-id}}))

(defn ^:private rename-changes
  [element definition references replacement replacement-raw db]
  (if (identical? :namespace-alias (:bucket element))
    (mapv (partial rename-alias replacement db) references)
    (condp identical? (:bucket definition)

      :namespace-definitions
      (mapv (partial rename-ns-definition replacement db) references)

      :keywords
      (mapv (partial rename-keyword replacement replacement-raw db) references)

      :locals
      (mapv (partial rename-local replacement db) references)

      (mapv (partial rename-other replacement db) references))))

(defn ^:private rename-status
  [element definition references source-paths client-capabilities]
  (cond
    (empty? references)
    {:error {:message "Can't rename, no other references found."
             :code :invalid-params}}

    (and (= :namespace-definitions (:bucket definition))
         (not= :namespace-alias (:bucket element))
         (seq source-paths)
         (not-any? #(string/starts-with? (:filename definition) %) source-paths))
    {:error {:code :invalid-params
             :message "Can't rename namespace, invalid source-paths. Are project :source-paths configured correctly?"}}

    (and (= :namespace-definitions (:bucket definition))
         (not= :namespace-alias (:bucket element))
         (not (get-in client-capabilities [:workspace :workspace-edit :document-changes])))
    {:error {:code :invalid-params
             :message "Can't rename namespace, client does not support file renames."}}

    (and (= :keywords (:bucket definition))
         (not (:ns definition)))
    {:error {:code :invalid-params
             :message "Can't rename, only namespaced keywords can be renamed."}}

    :else
    {:result :success}))

(defn prepare-rename
  [uri row col db]
  (let [filename (shared/uri->filename uri)
        element (q/find-element-under-cursor (:analysis @db) filename row col)
        references (q/find-references (:analysis @db) element true db)
        definition (q/find-definition (:analysis @db) element db)
        source-paths (settings/get db [:source-paths])
        client-capabilities (:client-capabilities @db)
        {:keys [error] :as result} (rename-status element definition references source-paths client-capabilities)]
    (if error
      result
      (shared/->range element))))

(defn rename
  [uri new-name row col db]
  (let [filename (shared/uri->filename uri)
        element (q/find-element-under-cursor (:analysis @db) filename row col)
        references (q/find-references (:analysis @db) element true db)
        definition (q/find-definition (:analysis @db) element db)
        source-paths (settings/get db [:source-paths])
        client-capabilities (:client-capabilities @db)
        {:keys [error] :as result} (rename-status element definition references source-paths client-capabilities)]
    (if error
      result
      (let [replacement (string/replace new-name #".*/([^/]*)$" "$1")
            changes (rename-changes element definition references replacement new-name db)
            doc-changes (->> changes
                             (group-by :text-document)
                             (remove (comp empty? val))
                             (map (fn [[text-document edits]]
                                    {:text-document text-document
                                     :edits (mapv #(dissoc % :text-document) edits)})))]
        (if (and (identical? :namespace-definitions (:bucket definition))
                 (not (identical? :namespace-alias (:bucket element))))
          (let [new-uri (shared/namespace->uri replacement source-paths (:filename definition) db)]
            (swap! db (fn [db] (-> db
                                   (update :documents #(set/rename-keys % {filename (shared/uri->filename new-uri)}))
                                   (update :analysis #(set/rename-keys % {filename (shared/uri->filename new-uri)})))))
            (shared/client-changes (concat doc-changes
                                           [{:kind "rename"
                                             :old-uri uri
                                             :new-uri new-uri}])
                                   db))
          (shared/client-changes doc-changes db))))))

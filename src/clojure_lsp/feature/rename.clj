(ns clojure-lsp.feature.rename
  (:require
   [clojure-lsp.feature.refactor :as f.refactor]
   [clojure-lsp.parser :as parser]
   [clojure-lsp.queries :as q]
   [clojure-lsp.shared :as shared]
   [clojure.set :as set]
   [clojure.string :as string]
   [taoensso.timbre :as log]))

(defn ^:private rename-keyword
  [replacement
   db
   {:keys [ns alias name filename
           name-col name-end-col
           namespace-from-prefix] :as reference}]
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
        text (cond
               alias
               (str "::" alias "/" replacement-name)

               qualified-same-ns?
               (str "::" replacement-name)

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

(defn ^:private rename-alias [replacement db reference]
  (let [alias? (= :namespace-alias (:bucket reference))
        keyword? (= :keywords (:bucket reference))
        ref-doc-uri (shared/filename->uri (:filename reference) db)
        [u-prefix _ u-name] (when-not alias?
                              (parser/ident-split (:name reference)))
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
  [definition references replacement db]
  (condp = (:bucket definition)

    :namespace-alias
    (mapv (partial rename-alias replacement db) references)

    :keywords
    (mapv (partial rename-keyword replacement db) references)

    :locals
    (mapv (partial rename-local replacement db) references)

    (mapv (partial rename-other replacement db) references)))

(defn ^:private can-rename?
  [definition references source-paths]
  (and (seq references)
       (or (not (seq source-paths))
           (some #(string/starts-with? (:filename definition) %) source-paths))
       (not (and (= :keywords (:bucket definition))
                 (not (:ns definition))))))

(defn rename
  [uri new-name row col db]
  (let [filename (shared/uri->filename uri)
        references (q/find-references-from-cursor (:analysis @db) filename row col true)
        definition (first (filter (comp #{:locals :var-definitions :namespace-definitions :namespace-alias :keywords} :bucket) references))
        source-paths (get-in @db [:settings :source-paths])]
    (when (can-rename? definition references source-paths)
      (let [replacement (string/replace new-name #".*/([^/]*)$" "$1")
            changes (rename-changes definition references replacement db)
            doc-changes (->> changes
                             (group-by :text-document)
                             (remove (comp empty? val))
                             (map (fn [[text-document edits]]
                                    {:text-document text-document
                                     :edits (mapv #(dissoc % :text-document) edits)})))]
        (if (and (= (:bucket definition) :namespace-definitions)
                 (get-in @db [:client-capabilities :workspace :workspace-edit :document-changes]))
          (let [new-uri (shared/namespace->uri replacement source-paths (:filename definition) db)]
            (swap! db (fn [db] (-> db
                                   (update :documents #(set/rename-keys % {filename (shared/uri->filename new-uri)}))
                                   (update :analysis #(set/rename-keys % {filename (shared/uri->filename new-uri)})))))
            (f.refactor/client-changes (concat doc-changes
                                               [{:kind "rename"
                                                 :old-uri uri
                                                 :new-uri new-uri}])
                                       db))
          (f.refactor/client-changes doc-changes db))))))

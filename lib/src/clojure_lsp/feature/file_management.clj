(ns clojure-lsp.feature.file-management
  (:require
   [babashka.fs :as fs]
   [clojure-lsp.clj-depend :as lsp.depend]
   [clojure-lsp.config :as config]
   [clojure-lsp.dep-graph :as dep-graph]
   [clojure-lsp.feature.completion-lib :as f.completion-lib]
   [clojure-lsp.feature.diagnostics :as f.diagnostic]
   [clojure-lsp.feature.diagnostics.built-in :as f.diagnostics.built-in]
   [clojure-lsp.feature.diagnostics.custom :as f.diagnostics.custom]
   [clojure-lsp.feature.rename :as f.rename]
   [clojure-lsp.kondo :as lsp.kondo]
   [clojure-lsp.producer :as producer]
   [clojure-lsp.queries :as q]
   [clojure-lsp.settings :as settings]
   [clojure-lsp.shared :as shared]
   [clojure.core.async :as async]
   [clojure.java.io :as io]
   [clojure.set :as set]
   [clojure.string :as string]
   [medley.core :as medley]))

(set! *warn-on-reflection* true)

(defn create-ns-changes [uri text db]
  (when-let [new-ns (and (string/blank? text)
                         (contains? #{:clj :cljs :cljc} (shared/uri->file-type uri))
                         (not (get (:create-ns-blank-files-denylist db) uri))
                         (shared/uri->namespace uri db))]
    (when (settings/get db [:auto-add-ns-to-new-files?] true)
      (let [new-text (format "(ns %s)" new-ns)
            changes [{:text-document {:version (get-in db [:documents uri :v] 0) :uri uri}
                      :edits [{:range (shared/->range {:row 1 :end-row 999999 :col 1 :end-col 999999})
                               :new-text new-text}]}]]
        (shared/client-changes changes db)))))

(defn load-document! [uri text db*]
  (swap! db* update-in [:documents uri] assoc :v 0 :text text :saved-on-disk false))

(defn did-open [uri text {:keys [db* producer edits-chan] :as components} allow-create-ns]
  (load-document! uri text db*)
  (let [kondo-result* (future (lsp.kondo/run-kondo-on-text! text uri db*))
        depend-result* (future (lsp.depend/analyze-uri! uri @db*))
        kondo-result @kondo-result*
        depend-result @depend-result*]
    (swap! db* (fn [state-db]
                 (-> state-db
                     (lsp.kondo/db-with-results kondo-result)
                     (lsp.depend/db-with-results depend-result)
                     (f.diagnostics.built-in/db-with-results #(f.diagnostics.built-in/analyze-uri! uri %))
                     (f.diagnostics.custom/db-with-results #(f.diagnostics.custom/analyze-uri! uri %)))))
    (f.diagnostic/publish-diagnostics! uri components))
  (when allow-create-ns
    (when-let [create-ns-edits (create-ns-changes uri text @db*)]
      (async/>!! edits-chan create-ns-edits)))
  (when (and (f.completion-lib/dep-file? uri)
             (not (:libs @f.completion-lib/libs*))
             (not (:api? @db*)))
    (producer/publish-progress producer nil "Fetching libs for completion" "fetch-libs")
    (f.completion-lib/fetch-libs!)
    (producer/publish-progress producer 100 nil "fetch-libs")))

(defn ^:private set-xor [a b]
  (into (set/difference a b)
        (set/difference b a)))

(defn ^:private find-changed-elems
  "Detect elements that went from 0 to 1 occurrence, or from 1 to 0
  occurrences."
  [old-local-buckets new-local-buckets bucket signature-fn]
  (let [signature-with-elem (fn [elem]
                              (with-meta (signature-fn elem) {:elem elem}))
        old-signs (into #{} (map signature-with-elem) (bucket old-local-buckets))
        new-signs (into #{} (map signature-with-elem) (bucket new-local-buckets))]
    (map (comp :elem meta)
         (set-xor old-signs new-signs))))

(def ^:private definition-signature
  "Attributes of a var definition that, if they change, merit re-linting files
  that use the var. This isn't the full list of attributets copied by kondo from
  defs to usages, but it includes the attributes we care about."
  (juxt :ns :name :private :fixed-arities :defined-by :defined-by->lint-as))

(defn reference-uris [uri db-before db-after]
  (let [old-local-buckets (get-in db-before [:analysis uri])
        new-local-buckets (get-in db-after [:analysis uri])
        changed-var-definitions (find-changed-elems old-local-buckets new-local-buckets :var-definitions definition-signature)
        changed-var-usages (find-changed-elems old-local-buckets new-local-buckets :var-usages q/var-usage-signature)
        changed-kw-usages (find-changed-elems old-local-buckets new-local-buckets :keyword-usages q/kw-signature)
        project-db (-> db-after
                       q/db-with-internal-analysis
                       ;; don't notify self
                       (update :analysis dissoc uri))
        var-dependent-uri (when (seq changed-var-definitions)
                            (let [def-signs (into #{}
                                                  (mapcat q/var-definition-signatures)
                                                  changed-var-definitions)
                                  usage-of-changed-def? (fn [var-usage]
                                                          (contains? def-signs (q/var-usage-signature var-usage)))]
                              (into #{}
                                    (keep (fn [[uri {:keys [var-usages]}]]
                                            (when (some usage-of-changed-def? var-usages)
                                              uri)))
                                    (q/uri-dependents-analysis project-db uri))))
        var-dependency-uris (when (seq changed-var-usages)
                              ;; If definition is in both a clj and cljs file, and this is a clj
                              ;; usage, we are careful to notify only the clj file. But, it wouldn't
                              ;; really hurt to notify both files. So, if it helps readability,
                              ;; maintenance, or symmetry with `var-dependent-uris`, we could look
                              ;; at just signature, not signature and lang.
                              (let [usage-signs->langs (->> changed-var-usages
                                                            (reduce (fn [result usage]
                                                                      (assoc result (q/var-usage-signature usage) (q/elem-langs usage)))
                                                                    {}))
                                    def-of-changed-usage? (fn [var-def]
                                                            (when-let [usage-langs (some usage-signs->langs
                                                                                         (q/var-definition-signatures var-def))]
                                                              (some usage-langs (q/elem-langs var-def))))]
                                (into #{}
                                      (keep (fn [[uri {:keys [var-definitions]}]]
                                              (when (some def-of-changed-usage? var-definitions)
                                                uri)))
                                      (q/uri-dependencies-analysis project-db uri))))
        kw-dependency-uris (when (seq changed-kw-usages)
                             (let [usage-signs (into #{}
                                                     (map q/kw-signature)
                                                     changed-kw-usages)
                                   def-of-changed-usage? (fn [kw-def]
                                                           (contains? usage-signs (q/kw-signature kw-def)))]
                               (into #{}
                                     (keep (fn [[uri {:keys [keyword-definitions]}]]
                                             (when (some def-of-changed-usage? keyword-definitions)
                                               uri)))
                                     (:analysis project-db))))]
    ;; TODO: see note on `notify-references` We may want to handle these
    ;; sets of files differently.
    (set/union (or var-dependent-uri #{})
               (or var-dependency-uris #{})
               (or kw-dependency-uris #{}))))

(defn analyze-reference-uris! [uris db*]
  (let [kondo-result (lsp.kondo/run-kondo-on-reference-uris! uris db*)]
    (swap! db* (fn [state-db]
                 (-> state-db
                     (lsp.kondo/db-with-results kondo-result)
                     (f.diagnostics.built-in/db-with-results #(f.diagnostics.built-in/analyze-uris! uris %)))))))

(defn ^:private notify-references [uri db-before db-after {:keys [db* producer] :as components}]
  (when (settings/get db-after [:notify-references-on-file-change] true)
    (async/thread
      (shared/logging-task
        :internal/notify-references
        (let [uris (shared/logging-task
                     :reference-files/find
                     (reference-uris uri db-before db-after))]
          (when (seq uris)
            (shared/logging-task
              :reference-files/analyze
              ;; TODO: We process the dependent and dependency files together, but
              ;; it may be possible to be more efficient by processing them
              ;; separately.
              ;;
              ;; The dependents may have been affected by changes to var
              ;; definitions. Since some var definition data is copied to var
              ;; usage data, this will change their analysis slightly. They may
              ;; also gain or lose clj-kondo lint like unresolved-var or
              ;; invalid-arity.
              ;;
              ;; The dependencies may have been affected by changes to var usages.
              ;; Their analysis won't have changed, but they may gain or lose
              ;; custom unused-public-var lint.
              ;;
              ;; So, we could send the dependents to kondo, bypassing custom-lint.
              ;; And we could send the dependencies to custom-lint, bypassing
              ;; kondo. See
              ;; https://github.com/clojure-lsp/clojure-lsp/issues/1027 and
              ;; https://github.com/clojure-lsp/clojure-lsp/issues/1028.
              (analyze-reference-uris! uris db*))
            (f.diagnostic/publish-all-diagnostics! uris true components)
            (producer/refresh-code-lens producer)))))))

(defn ^:private offsets [lines line character end-line end-character]
  (loop [lines (seq lines)
         offset 0
         idx 0]
    (if (or (not lines)
            (= line idx))
      [(+ offset character line)
       (loop [lines lines
              offset offset
              idx idx]
         (if (or (not lines)
                 (= end-line idx))
           (+ offset end-character end-line)
           (recur (next lines)
                  (+ offset (count (first lines)))
                  (inc idx))))]
      (recur (next lines)
             (+ offset (count (first lines)))
             (inc idx)))))

(defn replace-text
  "Returns ORIGINAL text but replacing its substring from LINE number at
  CHARACTER position to END-LINE number at END-CHARACTER position with
  REPLACEMENT text.

  All line endings of the returned string are replaced with that of
  the ORIGINAL's line ending type.

  The ORIGINAL's line ending type is set to be its first line ending
  type (either `\r\n` or `\n`), or, if no newline is found, the
  system's line separator."
  [original replacement line character end-line end-character]

  (let [original-nl (or (re-find #"\r\n|\n" original)
                        shared/line-separator)
        replacement-nl (re-find #"\r\n|\n" replacement)
        ;; normalize `\r\n`'s to `\n` so that `\r`'s do not creap in
        ;; as normal chars
        original (cond-> original
                   (= original-nl "\r\n") (string/replace #"\r\n" "\n"))
        replacement (cond-> replacement
                      (= replacement-nl "\r\n") (string/replace #"\r\n" "\n"))
        lines (string/split-lines original) ;; don't use OS specific line delimiter!
        [start-offset end-offset] (offsets lines line character end-line end-character)
        [prefix suffix] [(subs original 0 start-offset)
                         (subs original (min end-offset (count original)) (count original))]]
    ;; restore normalized string to ORIGINAL's line ending type.
    (cond-> (string/join [prefix replacement suffix])
      (= original-nl "\r\n") (string/replace #"\n" "\r\n"))))

(defn ^:private handle-change
  "Handle a TextDocumentContentChangeEvent"
  [old-text change]
  (let [new-text (:text change)]
    (if-let [r (:range change)]
      (let [{{start-line :line
              start-character :character} :start
             {end-line :line
              end-character :character} :end} r]
        (replace-text old-text new-text start-line start-character end-line end-character))
      ;; If range and rangeLength are omitted the new text is considered to be
      ;; the full content of the document.
      new-text)))

(defn ^:private bump-version
  "Advance the analyzed-version, but only if it's newer, in case analysis
  completes out of order."
  [old-version new-version]
  (if (or (not old-version)
          (< old-version new-version))
    new-version
    old-version))

(defn analyze-changes [{:keys [uri text version]} {:keys [producer db*] :as components}]
  (loop [state-db @db*]
    (when (>= version (get-in state-db [:documents uri :v] -1))
      (let [kondo-result* (future
                            (shared/logging-task
                              :internal/uri-analyzed-by-clj-kondo
                              (lsp.kondo/run-kondo-on-text! text uri db*)))
            depend-result* (future
                             (shared/logging-task
                               :internal/uri-analyzed-by-clj-depend
                               (lsp.depend/analyze-uri! uri state-db)))
            kondo-result @kondo-result*
            depend-result @depend-result*
            old-db @db*]
        (if (compare-and-set! db* state-db
                              (-> state-db
                                  (lsp.kondo/db-with-results kondo-result)
                                  (lsp.depend/db-with-results depend-result)
                                  (f.diagnostics.built-in/db-with-results #(f.diagnostics.built-in/analyze-uri! uri %))
                                  (f.diagnostics.custom/db-with-results #(f.diagnostics.custom/analyze-uri! uri %))
                                  (update-in [:documents uri :analyzed-version]
                                             bump-version version)))
          (let [db @db*]
            (f.diagnostic/publish-diagnostics! uri components)
            (notify-references uri old-db db components)
            (producer/refresh-test-tree producer [uri]))
          (recur @db*))))))

(defn did-change [uri changes version {:keys [db* current-changes-chan]}]
  (let [old-text (get-in @db* [:documents uri :text])
        final-text (reduce handle-change old-text changes)]
    (swap! db* (fn [state-db] (-> state-db
                                  (assoc-in [:documents uri :v] version)
                                  (assoc-in [:documents uri :text] final-text))))
    (async/>!! current-changes-chan {:uri uri
                                     :text final-text
                                     :version version})))

(defn analyze-watched-files! [uris {:keys [db* producer] :as components}]
  (let [old-db @db*
        existing-uris (->> uris
                           distinct
                           (filter #(shared/file-exists? (io/file (shared/uri->filename %))))) ;; we check if file still exists/should be linted
        filenames (map shared/uri->filename existing-uris)
        kondo-result (lsp.kondo/run-kondo-on-paths! filenames db* {:external? false} nil)]
    (swap! db* (fn [state-db]
                 (-> state-db
                     (lsp.kondo/db-with-results kondo-result)
                     (f.diagnostics.built-in/db-with-results #(f.diagnostics.built-in/analyze-uris! existing-uris %)))))
    (f.diagnostic/publish-all-diagnostics! uris true components)
    (producer/refresh-test-tree producer uris)
    (doseq [uri uris]
      (when (get-in @db* [:documents uri :v])
        (when-let [text (shared/slurp-uri uri)]
          (swap! db* assoc-in [:documents uri :text] text)))
      (notify-references uri old-db @db* components))))

(defn ^:private db-without-uri [state-db uri]
  (-> state-db
      (dep-graph/remove-doc uri)
      (shared/dissoc-in [:documents uri])
      (shared/dissoc-in [:analysis uri])
      (shared/dissoc-in [:diagnostics :clj-kondo uri])))

(defn ^:private files-deleted [old-db {:keys [db*] :as components} uris]
  (swap! db* #(reduce db-without-uri % uris))
  (f.diagnostic/publish-empty-diagnostics! uris components)
  (doseq [uri uris]
    (notify-references uri @db* old-db components)))

(defn ^:private dir-or-file-uri->analyzable-uris [uri db]
  ;; If the URI is for an entire directory that has been created/deleted, we
  ;; expand it.
  (let [source-paths-ignore-regexs (get-in db [:settings :source-paths-ignore-regex] config/default-source-path-ignore-regexs)
        root-path (shared/uri->path (:project-root-uri db))
        files (-> uri shared/uri->filename io/file file-seq)]
    (into []
          (comp
            ;; avoid watching ignored source-paths
            (remove (fn [file]
                      (let [relative-source-path (shared/relativize-filepath (str file) (str root-path))]
                        (some #(re-matches (re-pattern %) relative-source-path) source-paths-ignore-regexs))))
            (map #(.getAbsolutePath ^java.io.File %))
            (map #(shared/filename->uri % db))
            (remove #(identical? :unknown (shared/uri->file-type %))))
          files)))

(defn did-change-watched-files
  [changes
   {:keys [db* watched-files-chan] :as components}]
  (let [db @db*
        observe-changed? (settings/get db [:compute-external-file-changes] true)
        {:keys [created changed deleted]}
        (->> changes
             (group-by :type)
             (medley/map-vals
               (fn [changes]
                 (mapcat #(dir-or-file-uri->analyzable-uris (:uri %) db) changes))))]
    (doseq [created-or-changed (concat created (when observe-changed? changed))]
      (async/>!! watched-files-chan created-or-changed))
    (when (seq deleted)
      (shared/logging-task
        :internal/delete-watched-files
        (files-deleted db components deleted)))))

(defn did-close [uri {:keys [db*] :as components}]
  (let [db @db*
        filename (shared/uri->filename uri)
        source-paths (settings/get db [:source-paths])
        external-filename? (shared/external-filename? filename source-paths)]
    (if external-filename?
      (f.diagnostic/publish-empty-diagnostics! [uri] components)
      (when (not (shared/file-exists? (io/file filename)))
        (files-deleted db components [uri])))))

(defn force-get-document-text
  "Get document text from db, if document not found, tries to open the document"
  [uri {:keys [db*] :as components}]
  (or (get-in @db* [:documents uri :text])
      (when-let [text (shared/slurp-uri uri)]
        (did-open uri text components false)
        (get-in @db* [:documents uri :text]))))

(defn ^:private lint-opened-files-when-any-config-changed!
  [uri {:keys [db* producer] :as components}]
  (let [db @db*
        project-root-filename (shared/uri->filename (:project-root-uri db))
        config-files #{(io/file project-root-filename ".clj-kondo" "config.edn")
                       (io/file project-root-filename ".lsp" "config.edn")}
        config-file-saved? (some (comp #(= uri %)
                                       #(shared/filename->uri % db)
                                       str
                                       fs/canonicalize)
                                 config-files)]

    (when config-file-saved?
      (let [all-opened-uris (->> (:documents db)
                                 (keep (fn [[uri document]] (when (:v document) uri)))
                                 set)]
        (analyze-reference-uris! all-opened-uris db*)
        (f.diagnostic/publish-all-diagnostics! all-opened-uris true components)
        (producer/refresh-code-lens producer)))))

(defn did-save [uri {:keys [db*] :as components}]
  (swap! db* #(assoc-in % [:documents uri :saved-on-disk] true))
  (lint-opened-files-when-any-config-changed! uri components))

(defn will-rename-files [files db]
  (->> files
       (keep (fn [{:keys [old-uri new-uri]}]
               (let [new-ns (shared/uri->namespace new-uri db)
                     old-ns-definition (q/find-namespace-definition-by-uri db old-uri)]
                 (when (and new-ns
                            old-ns-definition
                            (not= new-ns (name (:name old-ns-definition))))
                   (f.rename/rename-element new-ns db old-ns-definition :rename-file)))))
       (reduce #(shared/deep-merge %1 %2) {:document-changes []})))

(defn did-rename-files [files {:keys [db*] :as components}]
  (files-deleted @db* components (mapv :old-uri files)))

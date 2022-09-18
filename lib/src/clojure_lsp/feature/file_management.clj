(ns clojure-lsp.feature.file-management
  (:require
   [babashka.fs :as fs]
   [clojure-lsp.clj-depend :as lsp.depend]
   [clojure-lsp.dep-graph :as dep-graph]
   [clojure-lsp.feature.diagnostics :as f.diagnostic]
   [clojure-lsp.feature.rename :as f.rename]
   [clojure-lsp.kondo :as lsp.kondo]
   [clojure-lsp.logger :as logger]
   [clojure-lsp.producer :as producer]
   [clojure-lsp.queries :as q]
   [clojure-lsp.settings :as settings]
   [clojure-lsp.shared :as shared]
   [clojure.core.async :as async]
   [clojure.java.io :as io]
   [clojure.set :as set]
   [clojure.string :as string]))

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

(defn did-open [uri text {:keys [db* edits-chan] :as components} allow-create-ns]
  (load-document! uri text db*)
  (let [kondo-result* (future (lsp.kondo/run-kondo-on-text! text uri db*))
        depend-result* (future (lsp.depend/analyze-filename! (shared/uri->filename uri) @db*))
        kondo-result @kondo-result*
        depend-result @depend-result*]
    (swap! db* (fn [state-db]
                 (-> state-db
                     (lsp.kondo/db-with-results kondo-result)
                     (lsp.depend/db-with-results depend-result))))
    (f.diagnostic/publish-diagnostics! uri components))
  (when allow-create-ns
    (when-let [create-ns-edits (create-ns-changes uri text @db*)]
      (async/>!! edits-chan create-ns-edits))))

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
  (juxt :ns :name :private :fixed-arities :defined-by))

(defn reference-filenames [filename db-before db-after]
  (let [uri (shared/filename->uri filename db-before)
        old-local-buckets (get-in db-before [:analysis filename])
        new-local-buckets (get-in db-after [:analysis filename])
        changed-var-definitions (find-changed-elems old-local-buckets new-local-buckets :var-definitions definition-signature)
        changed-var-usages (find-changed-elems old-local-buckets new-local-buckets :var-usages q/var-usage-signature)
        changed-kw-usages (find-changed-elems old-local-buckets new-local-buckets :keyword-usages q/kw-signature)
        project-db (-> db-after
                       q/db-with-internal-analysis
                       ;; don't notify self
                       (update :analysis dissoc filename))
        var-dependent-filenames (when (seq changed-var-definitions)
                                  (let [def-signs (into #{}
                                                        (mapcat q/var-definition-signatures)
                                                        changed-var-definitions)
                                        usage-of-changed-def? (fn [var-usage]
                                                                (contains? def-signs (q/var-usage-signature var-usage)))]
                                    (into #{}
                                          (keep (fn [[filename {:keys [var-usages]}]]
                                                  (when (some usage-of-changed-def? var-usages)
                                                    filename)))
                                          (q/uri-dependents-analysis project-db uri))))
        var-dependency-filenames (when (seq changed-var-usages)
                                   ;; If definition is in both a clj and cljs file, and this is a clj
                                   ;; usage, we are careful to notify only the clj file. But, it wouldn't
                                   ;; really hurt to notify both files. So, if it helps readability,
                                   ;; maintenance, or symmetry with `dependent-filenames`, we could look
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
                                           (keep (fn [[filename {:keys [var-definitions]}]]
                                                   (when (some def-of-changed-usage? var-definitions)
                                                     filename)))
                                           (q/uri-dependencies-analysis project-db uri))))
        kw-dependency-filenames (when (seq changed-kw-usages)
                                  (let [usage-signs (into #{}
                                                          (map q/kw-signature)
                                                          changed-kw-usages)
                                        def-of-changed-usage? (fn [kw-def]
                                                                (contains? usage-signs (q/kw-signature kw-def)))]
                                    (into #{}
                                          (keep (fn [[filename {:keys [keyword-definitions]}]]
                                                  (when (some def-of-changed-usage? keyword-definitions)
                                                    filename)))
                                          (:analysis project-db))))]
    ;; TODO: see note on `notify-references` We may want to handle these
    ;; sets of files differently.
    (set/union (or var-dependent-filenames #{})
               (or var-dependency-filenames #{})
               (or kw-dependency-filenames #{}))))

(defn analyze-reference-filenames! [filenames db*]
  (let [result (lsp.kondo/run-kondo-on-reference-filenames! filenames db*)]
    (swap! db* lsp.kondo/db-with-results result)))

(defn ^:private notify-references [filename db-before db-after {:keys [db* producer] :as components}]
  (async/thread
    (shared/logging-task
      :notify-references
      (let [filenames (shared/logging-task
                        :reference-files/find
                        (reference-filenames filename db-before db-after))]
        (when (seq filenames)
          (logger/debug "Analyzing references for files:" filenames)
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
            (analyze-reference-filenames! filenames db*))
          (f.diagnostic/publish-all-diagnostics! filenames components)
          (producer/refresh-code-lens producer))))))

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

(defn replace-text [original replacement line character end-line end-character]
  (let [lines (string/split original #"\n") ;; don't use OS specific line delimiter!
        [start-offset end-offset] (offsets lines line character end-line end-character)
        [prefix suffix] [(subs original 0 start-offset)
                         (subs original (min end-offset (count original)) (count original))]]
    (string/join [prefix replacement suffix])))

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

(defn analyze-changes [{:keys [uri text version]} {:keys [producer db*] :as components}]
  (let [filename (shared/uri->filename uri)]
    (loop [state-db @db*]
      (when (>= version (get-in state-db [:documents uri :v] -1))
        (let [kondo-result* (future
                              (shared/logging-time
                                (str "changes analyzed by clj-kondo took %s")
                                (lsp.kondo/run-kondo-on-text! text uri db*)))
              depend-result* (future
                               (shared/logging-time
                                 (str "changes analyzed by clj-depend took %s")
                                 (lsp.depend/analyze-filename! filename state-db)))
              kondo-result @kondo-result*
              depend-result @depend-result*
              old-db @db*]
          (if (compare-and-set! db* state-db
                                (-> state-db
                                    (lsp.kondo/db-with-results kondo-result)
                                    (lsp.depend/db-with-results depend-result)
                                    (update :processing-changes disj uri)))
            (let [db @db*]
              (f.diagnostic/publish-diagnostics! uri components)
              (when (settings/get db [:notify-references-on-file-change] true)
                (notify-references filename old-db db components))
              (producer/refresh-test-tree producer [uri]))
            (recur @db*)))))))

(defn did-change [uri changes version {:keys [db* current-changes-chan]}]
  (let [old-text (get-in @db* [:documents uri :text])
        final-text (reduce handle-change old-text changes)]
    (swap! db* (fn [state-db] (-> state-db
                                  (assoc-in [:documents uri :v] version)
                                  (assoc-in [:documents uri :text] final-text)
                                  (update :processing-changes conj uri))))
    (async/>!! current-changes-chan {:uri uri
                                     :text final-text
                                     :version version})))

(defn analyze-watched-files! [uris {:keys [db* producer] :as components}]
  (let [filenames (map shared/uri->filename uris)
        result (shared/logging-time
                 "Watched files analyzed, took %s"
                 (lsp.kondo/run-kondo-on-paths! filenames db* {:external? false} nil))]
    (swap! db* lsp.kondo/db-with-results result)
    (f.diagnostic/publish-all-diagnostics! filenames components)
    (producer/refresh-test-tree producer uris)
    (doseq [uri uris]
      (when (get-in @db* [:documents uri :v])
        (when-let [text (shared/slurp-uri uri)]
          (swap! db* assoc-in [:documents uri :text] text))))))

(defn ^:private db-without-file [state-db uri filename]
  (-> state-db
      (dep-graph/remove-file uri filename)
      (shared/dissoc-in [:documents uri])
      (shared/dissoc-in [:analysis filename])
      (shared/dissoc-in [:findings filename])))

(defn ^:private file-deleted [{:keys [db*], :as components} uri filename]
  (swap! db* db-without-file uri filename)
  (f.diagnostic/publish-empty-diagnostics! uri components))

(defn did-change-watched-files
  [changes
   {:keys [db* watched-files-chan] :as components}]
  (doseq [{:keys [uri type]} changes]
    (case type
      :created (async/>!! watched-files-chan uri)
      :changed (when (settings/get @db* [:compute-external-file-changes] true)
                 (async/>!! watched-files-chan uri))
      :deleted (shared/logging-task
                 :delete-watched-file
                 (file-deleted components uri (shared/uri->filename uri))))))

(defn did-close [uri {:keys [db*] :as components}]
  (let [filename (shared/uri->filename uri)
        source-paths (settings/get @db* [:source-paths])
        external-filename? (shared/external-filename? filename source-paths)]
    (when external-filename?
      (f.diagnostic/publish-empty-diagnostics! uri components))
    (when (and (not external-filename?)
               (not (shared/file-exists? (io/file filename))))
      (file-deleted components uri filename))))

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
                                       fs/canonicalize)
                                 config-files)]

    (when config-file-saved?
      (let [all-opened-filenames (->> (:documents db)
                                      (keep (fn [[uri document]]
                                              (when (:v document)
                                                (shared/uri->filename uri))))
                                      set)]
        (analyze-reference-filenames! all-opened-filenames db*)
        (f.diagnostic/publish-all-diagnostics! all-opened-filenames components)
        (producer/refresh-code-lens producer)))))

(defn did-save [uri {:keys [db*] :as components}]
  (swap! db* #(assoc-in % [:documents uri :saved-on-disk] true))
  (lint-opened-files-when-any-config-changed! uri components))

(defn will-rename-files [files db]
  (->> files
       (keep (fn [{:keys [old-uri new-uri]}]
               (let [old-filename (shared/uri->filename old-uri)
                     new-ns (shared/uri->namespace new-uri db)
                     old-ns-definition (q/find-namespace-definition-by-filename db old-filename)]
                 (when (and new-ns
                            old-ns-definition
                            (not= new-ns (name (:name old-ns-definition))))
                   (f.rename/rename-element old-uri new-ns db old-ns-definition :rename-file)))))
       (reduce #(shared/deep-merge %1 %2) {:document-changes []})))

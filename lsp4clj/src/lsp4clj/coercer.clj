(ns lsp4clj.coercer
  (:require
   [clojure.data.json :as json]
   [clojure.java.data :as j]
   [clojure.spec.alpha :as s]
   [clojure.string :as string]
   [clojure.walk :as walk]
   [lsp4clj.protocols.logger :as logger]
   [medley.core :as medley])
  (:import
   (com.google.gson JsonElement)
   (lsp4clj.protocols.logger ILSPLogger)
   (org.eclipse.lsp4j
     CallHierarchyIncomingCall
     CallHierarchyOutgoingCall
     CallHierarchyItem
     CodeAction
     CodeActionOptions
     CodeActionKind
     CodeLens
     CodeLensOptions
     Command
     CompletionItem
     CompletionOptions
     CompletionItemKind
     Diagnostic
     DiagnosticSeverity
     DocumentHighlight
     DocumentSymbol
     ExecuteCommandOptions
     FileChangeType
     Hover
     InsertTextFormat
     Location
     MarkedString
     MarkupContent
     MessageActionItem
     MessageParams
     ShowMessageRequestParams
     MessageType
     LinkedEditingRanges
     ParameterInformation
     Position
     ProgressParams
     PublishDiagnosticsParams
     Range
     CreateFileOptions
     CreateFile
     RenameFile
     RenameOptions
     ResponseErrorCode
     PrepareRenameResult
     SaveOptions
     SemanticTokens
     SemanticTokensLegend
     SemanticTokensWithRegistrationOptions
     ServerCapabilities
     ShowDocumentParams
     SignatureHelp
     SignatureHelpOptions
     SignatureInformation
     SymbolKind
     SymbolInformation
     TextDocumentEdit
     TextDocumentIdentifier
     TextDocumentSyncKind
     TextDocumentSyncOptions
     TextEdit
     VersionedTextDocumentIdentifier
     WorkDoneProgressBegin
     WorkDoneProgressKind
     WorkDoneProgressNotification
     WorkDoneProgressReport
     WorkDoneProgressEnd
     WorkspaceEdit)
   (org.eclipse.lsp4j.jsonrpc
     ResponseErrorException)
   (org.eclipse.lsp4j.jsonrpc.messages
     Either
     ResponseError)))

(set! *warn-on-reflection* true)

(defn ^{:deprecated "use java->clj instead"} debeaner [inst]
  (when inst
    (->> (dissoc (bean inst) :class)
         (into {})
         (medley/remove-vals nil?)
         (medley/map-keys #(as-> % map-key
                             (name map-key)
                             (string/split map-key #"(?=[A-Z])")
                             (string/join "-" map-key)
                             (string/lower-case map-key)
                             (keyword map-key))))))

(def watched-files-type-enum {1 :created 2 :changed 3 :deleted})

(defn document->uri [^TextDocumentIdentifier document]
  (.getUri document))

(defmethod j/from-java DiagnosticSeverity [^DiagnosticSeverity instance]
  (-> instance .name .toLowerCase keyword))

(defmethod j/from-java FileChangeType [^FileChangeType instance]
  (get watched-files-type-enum (.getValue instance)))

(defmethod j/from-java MessageType [^MessageType instance]
  (-> instance .name .toLowerCase keyword))

(defmethod j/from-java CompletionItemKind [^CompletionItemKind instance]
  (-> instance .name .toLowerCase keyword))

(defmethod j/from-java InsertTextFormat [^InsertTextFormat instance]
  (-> instance .name .toLowerCase keyword))

(defmethod j/from-java SymbolKind [^SymbolKind instance]
  (-> instance .name .toLowerCase keyword))

(defmethod j/from-java Either [^Either instance]
  (j/from-java (.get instance)))

(defmethod j/from-java TextDocumentIdentifier [^TextDocumentIdentifier instance]
  (document->uri instance))

(defmethod j/from-java VersionedTextDocumentIdentifier [^VersionedTextDocumentIdentifier instance]
  {:version (.getVersion instance)
   :uri (document->uri instance)})

(defmethod j/from-java JsonElement [^JsonElement instance]
  (-> instance
      .toString
      json/read-str
      walk/keywordize-keys))

(defn respond-with-error [e]
  (let [error (ResponseError. (.getValue ^ResponseErrorCode (:code e))
                              ^String (:message e)
                              nil)]
    (throw (ResponseErrorException. error))))

(def error-code-enum
  {:invalid-params ResponseErrorCode/InvalidParams})

(s/def :error/code (s/and keyword?
                          error-code-enum
                          (s/conformer #(get error-code-enum %))))
(s/def :error/message string?)

(s/def ::error (s/and (s/keys :req-un [:error/code :error/message])
                      (s/conformer respond-with-error)))

(s/def ::response-error (s/and (s/keys :req-un [::error])))

(s/def ::line (s/and integer? (s/conformer int)))
(s/def ::character (s/and integer? (s/conformer int)))
(s/def ::position (s/and (s/keys :req-un [::line ::character])
                         (s/conformer #(Position. (:line %1) (:character %1)))))
(s/def ::start ::position)
(s/def ::end ::position)
(s/def ::range (s/and (s/keys :req-un [::start ::end])
                      (s/conformer #(Range. (:start %1) (:end %1)))))
(s/def ::selection-range ::range)

(def completion-kind-enum
  {:text 1 :method 2 :function 3 :constructor 4 :field 5 :variable 6 :class 7 :interface 8 :module 9
   :property 10 :unit 11 :value 12 :enum 13 :keyword 14 :snippet 15 :color 16 :file 17 :reference 18
   :folder 19 :enummember 20 :constant 21 :struct 22 :event 23 :operator 24 :typeparameter 25})

(s/def :completion-item/kind (s/and keyword?
                                    completion-kind-enum
                                    (s/conformer (fn [v] (CompletionItemKind/forValue (get completion-kind-enum v))))))

(def insert-text-format-enum
  {:plaintext 1
   :snippet 2})

(s/def :completion-item/insert-text-format
  (s/and keyword?
         insert-text-format-enum
         (s/conformer (fn [v] (InsertTextFormat/forValue (get insert-text-format-enum v))))))

(s/def ::new-text string?)
(s/def ::text-edit (s/and (s/keys :req-un [::new-text ::range])
                          (s/conformer #(TextEdit. (:range %1) (:new-text %1)))))
(s/def ::additional-text-edits (s/coll-of ::text-edit))
(s/def ::documentation (s/and (s/or :string string?
                                    :markup-content ::markup-content)
                              (s/conformer second)))

(s/def :prepare-rename/placeholder string?)
(s/def ::prepare-rename (s/and (s/keys :req-un [:prepare-rename/placeholder ::range])
                               (s/conformer #(PrepareRenameResult. (:range %) (:placeholder %)))))

(s/def ::prepare-rename-or-error
  (s/and (s/or :error ::response-error
               :range ::prepare-rename
               :start ::range)
         (s/conformer second)))

(s/def ::completion-item (s/and (s/keys :req-un [::label]
                                        :opt-un [::additional-text-edits ::filter-text ::detail ::text-edit
                                                 :completion-item/kind ::documentation ::data
                                                 ::insert-text :completion-item/insert-text-format])
                                (s/conformer (fn [{:keys [label additional-text-edits filter-text
                                                          detail text-edit kind documentation data
                                                          insert-text insert-text-format]}]
                                               (let [item (CompletionItem. label)
                                                     with-typed-docs (fn [^CompletionItem item]
                                                                       (if (instance? MarkupContent documentation)
                                                                         (.setDocumentation item ^MarkupContent documentation)
                                                                         (.setDocumentation item ^String documentation)))]
                                                 (cond-> item
                                                   filter-text (doto (.setFilterText filter-text))
                                                   kind (doto (.setKind kind))
                                                   text-edit (doto (.setTextEdit (Either/forLeft text-edit)))
                                                   additional-text-edits (doto (.setAdditionalTextEdits additional-text-edits))
                                                   insert-text (doto (.setInsertText insert-text))
                                                   insert-text-format (doto (.setInsertTextFormat insert-text-format))
                                                   detail (doto (.setDetail detail))
                                                   documentation (doto with-typed-docs)
                                                   data (doto (.setData data))))))))

(s/def ::completion-items (s/coll-of ::completion-item))
(s/def ::version (s/and integer? (s/conformer int)))
(s/def ::uri string?)
(s/def ::edits (s/coll-of ::text-edit))
(s/def ::text-document (s/and (s/keys :req-un [::version ::uri])
                              (s/conformer #(VersionedTextDocumentIdentifier. (:uri %) (:version %)))))
(s/def ::text-document-edit (s/and (s/keys :req-un [::text-document ::edits])
                                   (s/conformer #(TextDocumentEdit. (:text-document %1) (:edits %1)))))
(s/def ::changes (s/coll-of (s/tuple string? ::edits) :kind map?))

(s/def :create-file/options (s/and (s/keys :opt-un [::overwrite? ::ignore-if-exists?])
                                   (s/conformer #(CreateFileOptions. (boolean (:overwrite? %))
                                                                     (boolean (:ignore-if-exists? %))))))

(s/def :create-file/kind (s/and string?
                                #(= % "create")))
(s/def ::create-file (s/and (s/keys :req-un [:create-file/kind ::uri]
                                    :opt-un [:create-file/options])
                            (s/conformer #(CreateFile. (:uri %) (:options %)))))
(s/def :rename-file/kind (s/and string?
                                #(= % "rename")))
(s/def :rename-file/old-uri ::uri)
(s/def :rename-file/new-uri ::uri)

(s/def ::rename-file (s/and (s/keys :req-un [:rename-file/kind :rename-file/old-uri :rename-file/new-uri])
                            (s/conformer #(RenameFile. (:old-uri %) (:new-uri %)))))

(s/def ::document-changes-entry (s/or :create-file ::create-file
                                      :rename-file ::rename-file
                                      :text-document-edit ::text-document-edit))
(s/def ::document-changes (s/and (s/coll-of ::document-changes-entry)
                                 (s/conformer #(map (fn [c]
                                                      (case (first c)
                                                        :text-document-edit (Either/forLeft (second c))
                                                        :create-file (Either/forRight (second c))
                                                        :rename-file (Either/forRight (second c))))
                                                    %))))

(s/def ::workspace-edit
  (s/and (s/keys :opt-un [::document-changes ::changes])
         (s/conformer #(if-let [changes (:changes %)]
                         (WorkspaceEdit. ^java.util.Map changes)
                         (WorkspaceEdit. ^java.util.List (:document-changes %))))))

(s/def ::workspace-edit-or-error
  (s/and (s/or :error ::response-error
               :changes ::workspace-edit
               :document-changes ::workspace-edit)
         (s/conformer second)))

(s/def ::location (s/and (s/keys :req-un [::uri ::range])
                         (s/conformer #(Location. (:uri %1) (:range %1)))))
(s/def ::locations (s/coll-of ::location))

(s/def :signature-help/documentation ::documentation)

(s/def :signature-help/parameter (s/and (s/keys :req-un [::label]
                                                :opt-un [:signature-help/documentation])
                                        (s/conformer (fn [{:keys [label documentation]}]
                                                       (let [parameter (ParameterInformation. label)
                                                             with-typed-docs (fn [^ParameterInformation parameter]
                                                                               (if (instance? MarkupContent documentation)
                                                                                 (.setDocumentation parameter ^MarkupContent documentation)
                                                                                 (.setDocumentation parameter ^String documentation)))]
                                                         (cond-> parameter
                                                           documentation (doto with-typed-docs)))))))

(s/def :signature-help/parameters (s/coll-of :signature-help/parameter))

(s/def :signature-help/signature-information (s/and (s/keys :req-un [::label]
                                                            :opt-un [:signature-help/documentation :signature-help/parameters :signature-help/active-parameter])
                                                    (s/conformer (fn [{:keys [label documentation parameters active-parameter]}]
                                                                   (let [info (SignatureInformation. label)
                                                                         with-typed-docs (fn [^SignatureInformation info]
                                                                                           (if (instance? MarkupContent documentation)
                                                                                             (.setDocumentation info ^MarkupContent documentation)
                                                                                             (.setDocumentation info ^String documentation)))]
                                                                     (cond-> info
                                                                       :always (doto (.setParameters parameters)
                                                                                 (.setActiveParameter active-parameter))
                                                                       documentation (doto with-typed-docs)))))))

(s/def :signature-help/signatures (s/coll-of :signature-help/signature-information))

(s/def ::signature-help (s/and (s/keys :req-un [:signature-help/signatures]
                                       :opt-un [:signature-help/active-signature :signature-help/active-parameter])
                               (s/conformer #(doto (SignatureHelp.)
                                               (.setSignatures (:signatures %))
                                               (.setActiveSignature (some-> % :active-signature int))
                                               (.setActiveParameter (some-> % :active-parameter int))))))

(def symbol-kind-enum
  {:file 1 :module 2 :namespace 3 :package 4 :class 5 :method 6 :property 7 :field 8 :constructor 9
   :enum 10 :interface 11 :function 12 :variable 13 :constant 14 :string 15 :number 16 :boolean 17
   :array 18 :object 19 :key 20 :null 21 :enum-member 22 :struct 23 :event 24 :operator 25
   :type-parameter 26})

(s/def :symbol/kind (s/and keyword?
                           symbol-kind-enum
                           (s/conformer (fn [v] (SymbolKind/forValue (get symbol-kind-enum v))))))

(s/def :document-symbol/selection-range ::range)

(s/def :document-symbol/detail string?)

(s/def ::document-symbol (s/and (s/keys :req-un [::name :symbol/kind ::range :document-symbol/selection-range]
                                        :opt-un [:document-symbol/detail :document-symbol/children])
                                (s/conformer (fn [m]
                                               (DocumentSymbol. (:name m) (:kind m) (:range m)
                                                                (:selection-range m) (:detail m) (:children m))))))

(s/def :document-symbol/children (s/coll-of ::document-symbol))

(s/def ::document-symbols (s/and (s/coll-of ::document-symbol)
                                 (s/conformer (fn [c]
                                                (map #(Either/forRight %) c)))))

(s/def ::document-highlight (s/and (s/keys :req-un [::range])
                                   (s/conformer (fn [m]
                                                  (DocumentHighlight. (:range m))))))

(s/def ::document-highlights (s/coll-of ::document-highlight))

(s/def ::symbol-information (s/and (s/keys :req-un [::name :symbol/kind ::location])
                                   (s/conformer (fn [m]
                                                  (SymbolInformation. (:name m) (:kind m) (:location m))))))

(s/def ::workspace-symbols (s/coll-of ::symbol-information))

(s/def ::severity (s/and integer?
                         (s/conformer #(DiagnosticSeverity/forValue %1))))

(s/def ::code (s/conformer name))

(s/def ::diagnostic (s/and (s/keys :req-un [::range ::message]
                                   :opt-un [::severity ::code ::tag ::source ::message])
                           (s/conformer #(doto (Diagnostic. (:range %1) (:message %1) (:severity %1) (:source %1) (:code %1))
                                           (.setTags (:tags %1))))))
(s/def ::diagnostics (s/coll-of ::diagnostic))
(s/def ::publish-diagnostics-params (s/and (s/keys :req-un [::uri ::diagnostics])
                                           (s/conformer #(PublishDiagnosticsParams. (:uri %1) (:diagnostics %1)))))

(s/def ::marked-string (s/and (s/or :string string?
                                    :marked-string (s/and (s/keys :req-un [::language ::value])
                                                          (s/conformer #(MarkedString. (:language %1) (:value %1)))))
                              (s/conformer (fn [v]
                                             (case (first v)
                                               :string (Either/forLeft (second v))
                                               :marked-string (Either/forRight (second v)))))))

(s/def :markup/kind #{"plaintext" "markdown"})
(s/def :markup/value string?)
(s/def ::markup-content (s/and (s/keys :req-un [:markup/kind :markup/value])
                               (s/conformer #(doto (MarkupContent.)
                                               (.setKind (:kind %1))
                                               (.setValue (:value %1))))))

(s/def ::contents (s/and (s/or :marked-strings (s/coll-of ::marked-string)
                               :markup-content ::markup-content)
                         (s/conformer second)))

(s/def ::hover (s/and (s/keys :req-un [::contents]
                              :opt-un [::range])
                      (s/conformer (fn [hover]
                                     (let [contents (:contents hover)
                                           range ^Range (:range hover)]
                                       (if (instance? MarkupContent contents)
                                         (Hover. ^MarkupContent contents
                                                 range)
                                         (Hover. ^java.util.List contents
                                                 range)))))))

(s/def :command/title string?)
(s/def :command/command string?)
(s/def :command/arguments (s/coll-of any?))

(s/def ::command (s/and (s/keys :req-un [:command/title :command/command]
                                :opt-un [:command/arguments])
                        (s/conformer #(Command. (:title %1) (:command %1) (:arguments %1)))))

(def show-message-type-enum
  {:error MessageType/Error
   :warning MessageType/Warning
   :info MessageType/Info
   :log MessageType/Log})

(s/def :show-message/type (s/and keyword?
                                 show-message-type-enum
                                 (s/conformer #(get show-message-type-enum %))))

(s/def :show-message/message string?)

(s/def ::show-message (s/and (s/keys :req-un [:show-message/type
                                              :show-message/message])
                             (s/conformer #(MessageParams. (:type %) (:message %)))))

(s/def :show-message-request-action/title string?)

(s/def :show-message-request/action (s/and (s/keys :req-un [:show-message-request-action/title])
                                           (s/conformer #(MessageActionItem. (:title %)))))

(s/def :show-message-request/actions (s/coll-of :show-message-request/action))

(s/def ::show-message-request (s/and (s/keys :req-un [:show-message/type
                                                      :show-message/message]
                                             :opt-un [:show-message-request/actions])
                                     (s/conformer #(doto (ShowMessageRequestParams.)
                                                     (.setMessage (:message %))
                                                     (.setType (:type %))
                                                     (.setActions (:actions %))))))

(def work-done-progress-kind-enum
  {:begin WorkDoneProgressKind/begin
   :report WorkDoneProgressKind/report
   :end WorkDoneProgressKind/end})

(s/def :work-done-progress/kind (s/and keyword?
                                       work-done-progress-kind-enum))

(s/def ::work-done-progress (s/and (s/keys :req-un [:work-done-progress/kind])
                                   (s/conformer (fn [w]
                                                  (case (:kind w)
                                                    :begin (doto (WorkDoneProgressBegin.)
                                                             (.setTitle (:title w))
                                                             (.setCancellable (:cancelable w))
                                                             (.setMessage (:message w))
                                                             (.setPercentage (int (:percentage w))))
                                                    :report (doto (WorkDoneProgressReport.)
                                                              (.setCancellable (:cancelable w))
                                                              (.setMessage (:message w))
                                                              (.setPercentage (int (:percentage w))))
                                                    :end (doto (WorkDoneProgressEnd.)
                                                           (.setMessage (:message w))))))))

(s/def :progress/token string?)

(s/def :progress/value ::work-done-progress)

(s/def ::notify-progress (s/and (s/keys :req-un [:progress/token
                                                 :progress/value])
                                (s/conformer #(ProgressParams. (Either/forLeft ^String (:token %))
                                                               (Either/forLeft ^WorkDoneProgressNotification (:value %))))))

(s/def ::show-document-request
  (s/and (s/keys :req-un [::uri ::range]
                 :opt-un [::take-focus?])
         (s/conformer #(doto (ShowDocumentParams. (:uri %))
                         (.setTakeFocus (:take-focus? %))
                         (.setSelection (:range %))))))

(s/def :code-action/title string?)

(s/def :code-action/edit ::workspace-edit-or-error)

(def code-action-kind
  {:quick-fix CodeActionKind/QuickFix
   :refactor CodeActionKind/Refactor
   :refactor-extract CodeActionKind/RefactorExtract
   :refactor-inline CodeActionKind/RefactorInline
   :refactor-rewrite CodeActionKind/RefactorRewrite
   :source CodeActionKind/Source
   :source-organize-imports CodeActionKind/SourceOrganizeImports})

(s/def :code-action/preferred? boolean?)

(s/def :code-action/kind (s/and (s/or :keyword (s/and keyword?
                                                      code-action-kind
                                                      (s/conformer #(get code-action-kind %)))
                                      :string (s/and string?
                                                     (s/conformer identity)))
                                (s/conformer second)))

(s/def ::code-action (s/and (s/keys :req-un [:code-action/title]
                                    :opt-un [:code-action/kind ::diagnostics :code-action/edit ::command :code-action/preferred? ::data])
                            (s/conformer #(doto (CodeAction. (:title %1))
                                            (.setKind (:kind %1))
                                            (.setDiagnostics (:diagnostics %1))
                                            (.setIsPreferred (:preferred? %1))
                                            (.setEdit (:edit %1))
                                            (.setCommand (:command %1))
                                            (.setData (walk/stringify-keys (:data %1)))))))

(s/def ::code-actions (s/coll-of ::code-action))

(s/def ::code-lens (s/and (s/keys :req-un [::range]
                                  :opt-un [::command ::data])
                          (s/conformer #(doto (CodeLens.)
                                          (.setRange (:range %1))
                                          (.setCommand (:command %1))
                                          (.setData (:data %1))))))

(s/def ::code-lenses (s/coll-of ::code-lens))

(s/def ::semantic-tokens (s/and (s/keys :req-un [::data]
                                        :opt-un [::result-id])
                                (s/conformer #(doto (SemanticTokens. (:result-id %1)
                                                                     (java.util.ArrayList. ^clojure.lang.PersistentVector (:data %1)))))))

(s/def ::call-hierarchy-item (s/and (s/keys :req-un [::name :symbol/kind ::uri ::range ::selection-range]
                                            :opt-un [::tags ::detail ::data])
                                    (s/conformer #(doto (CallHierarchyItem.)
                                                    (.setName (:name %1))
                                                    (.setKind (:kind %1))
                                                    (.setUri (:uri %1))
                                                    (.setRange (:range %1))
                                                    (.setSelectionRange (:selection-range %1))
                                                    (.setTags (:tags %1))
                                                    (.setDetail (:detail %1))
                                                    (.setData (:data %1))))))

(s/def ::call-hierarchy-items (s/coll-of ::call-hierarchy-item))

(s/def :call-hierarchy/from-ranges (s/coll-of ::range))
(s/def :call-hierarchy/from ::call-hierarchy-item)
(s/def :call-hierarchy/to ::call-hierarchy-item)

(s/def ::call-hierarchy-incoming-call (s/and (s/keys :req-un [:call-hierarchy/from :call-hierarchy/from-ranges])
                                             (s/conformer #(doto (CallHierarchyIncomingCall.)
                                                             (.setFrom (:from %1))
                                                             (.setFromRanges (:from-ranges %1))))))

(s/def ::call-hierarchy-outgoing-call (s/and (s/keys :req-un [:call-hierarchy/to :call-hierarchy/from-ranges])
                                             (s/conformer #(doto (CallHierarchyOutgoingCall.)
                                                             (.setTo (:to %1))
                                                             (.setFromRanges (:from-ranges %1))))))

(s/def ::call-hierarchy-incoming-calls (s/coll-of ::call-hierarchy-incoming-call))
(s/def ::call-hierarchy-outgoing-calls (s/coll-of ::call-hierarchy-outgoing-call))

(s/def :linked-editing-range/ranges (s/coll-of ::range))

(s/def ::linked-editing-ranges
  (s/and (s/keys :req-un [:linked-editing-range/ranges]
                 :opt-un [::word-pattern])
         (s/conformer #(doto (LinkedEditingRanges.)
                         (.setRanges (:ranges %1))
                         (.setWordPattern (:word-pattern %1))))))

(s/def ::linked-editing-ranges-or-error
  (s/and (s/or :error ::response-error
               :ranges ::linked-editing-ranges)
         (s/conformer second)))

(defn stringify-keys-and-vals
  "Recursively transforms all map keys and values from keywords to strings."
  [m]
  (let [kf (fn [[k v]] (if (keyword? k) [(name k) v] [k v]))
        vf (fn [[k v]] (if (keyword? v) [k (name v)] [k v]))]
    ;; only apply to maps
    (clojure.walk/postwalk
      (fn [x]
        (cond
          (symbol? x)
          (str x)

          (keyword? x)
          (name x)

          (map? x)
          (into {} (map #(-> % kf vf) x))

          :else
          x)) m)))

(defn clj->java [clj-map]
  (->> clj-map
       stringify-keys-and-vals
       (j/to-java java.util.Map)))

(defn java->clj [inst]
  (let [converted (j/from-java inst)]
    (if (map? converted)
      (->> converted
           (remove #(nil? (val %)))
           (into {}))
      converted)))

#_{:clj-kondo/ignore [:deprecated-var]}
(s/def ::legacy-debean (s/conformer debeaner))
(s/def ::debean (s/conformer java->clj))
(s/def ::bean (s/conformer clj->java))

(s/def :capabilities/code-action ::legacy-debean)
(s/def :capabilities/code-lens ::legacy-debean)
(s/def :capabilities/color-provider ::legacy-debean)
(s/def :capabilities/completion-item ::legacy-debean)
(s/def :capabilities/definition ::legacy-debean)
(s/def :capabilities/document-highlight ::legacy-debean)
(s/def :capabilities/document-link ::legacy-debean)
(s/def :capabilities/formatting ::legacy-debean)
(s/def :capabilities/implementation ::legacy-debean)
(s/def :capabilities/on-type-formatting ::legacy-debean)
(s/def :capabilities/publish-diagnostics ::legacy-debean)
(s/def :capabilities/range-formatting ::legacy-debean)
(s/def :capabilities/references ::legacy-debean)
(s/def :capabilities/rename ::legacy-debean)
(s/def :capabilities/signature-information ::debean)
(s/def :capabilities/synchronization ::legacy-debean)
(s/def :capabilities/type-definition ::legacy-debean)

(s/def :capabilities/symbol-kind-value-set
  (s/conformer (fn [value-set]
                 (set (map (fn [^SymbolKind kind]
                             (.getValue kind)) value-set)))))

(s/def :capabilities/symbol-kind (s/and ::legacy-debean
                                        (s/keys :opt-un [:capabilities/symbol-kind-value-set])))
(s/def :capabilities/document-symbol (s/and ::legacy-debean
                                            (s/keys :opt-un [:capabilities/symbol-kind])))
(s/def :capabilities/signature-help (s/and ::debean
                                           (s/keys :opt-un [:capabilities/signature-information])))

(s/def :capabilities/completion-item-kind-value-set
  (s/conformer (fn [value-set]
                 (set (map (fn [^CompletionItemKind kind]
                             (.getValue kind)) value-set)))))

(s/def :capabilities/completion-item-kind (s/and ::legacy-debean
                                                 (s/keys :opt-un [:capabilities/completion-item-kind-value-set])))
(s/def :capabilities/completion (s/and ::legacy-debean
                                       (s/keys :opt-un [:capabilities/completion-item
                                                        :capabilities/completion-item-kind])))
(s/def :capabilities/hover (s/and ::legacy-debean
                                  (s/keys :opt-un [:capabilities/content-format])))
(s/def :capabilities/text-document (s/and ::legacy-debean
                                          (s/keys :opt-un [:capabilities/hover
                                                           :capabilities/completion
                                                           :capabilities/definition
                                                           :capabilities/formatting
                                                           :capabilities/publish-diagnostics
                                                           :capabilities/code-action
                                                           :capabilities/document-symbol
                                                           :capabilities/code-lens
                                                           :capabilities/document-highlight
                                                           :capabilities/color-provider
                                                           :capabilities/type-definition
                                                           :capabilities/rename
                                                           :capabilities/references
                                                           :capabilities/document-link
                                                           :capabilities/synchronization
                                                           :capabilities/range-formatting
                                                           :capabilities/on-type-formatting
                                                           :capabilities/signature-help
                                                           :capabilities/implementation])))

(s/def :capabilities/workspace-edit ::legacy-debean)
(s/def :capabilities/did-change-configuration ::legacy-debean)
(s/def :capabilities/did-change-watched-files ::legacy-debean)
(s/def :capabilities/execute-command ::legacy-debean)
(s/def :capabilities/symbol (s/and ::legacy-debean
                                   (s/keys :opt-un [:capabilities/symbol-kind])))
(s/def :capabilities/workspace (s/and ::legacy-debean
                                      (s/keys :opt-un [:capabilities/workspace-edit
                                                       :capabilities/did-change-configuration
                                                       :capabilities/did-change-watched-files
                                                       :capabilities/execute-command
                                                       :capabilities/symbol])))
(s/def ::client-capabilities (s/and ::legacy-debean
                                    (s/keys :opt-un [:capabilities/workspace :capabilities/text-document])))

(s/def ::signature-help-provider (s/conformer #(cond (vector? %) (SignatureHelpOptions. %)
                                                     (map? %) (SignatureHelpOptions. (:trigger-characters %) (:retrigger-characters %))
                                                     :else (SignatureHelpOptions. %))))
(s/def ::code-action-provider (s/conformer #(when (vector? %) (CodeActionOptions. %))))
(s/def ::completion-provider (s/conformer #(when (map? %) (CompletionOptions. (:resolve-provider %1) (:trigger-characters %1)))))
(s/def ::execute-command-provider (s/conformer #(when (vector? %) (doto (ExecuteCommandOptions.)
                                                                    (.setCommands %)))))
(s/def ::semantic-tokens-provider (s/conformer #(when (and (:token-types %)
                                                           (:token-modifiers %)) (doto (SemanticTokensWithRegistrationOptions.)
                                                                                   (.setLegend (doto (SemanticTokensLegend.
                                                                                                       (:token-types %)
                                                                                                       (:token-modifiers %))))
                                                                                   (.setRange (:range %))
                                                                                   (.setFull ^Boolean (get % :full false))))))
(s/def ::text-document-sync (s/conformer #(doto (TextDocumentSyncOptions.)
                                            (.setOpenClose true)
                                            (.setChange (case %
                                                          :full TextDocumentSyncKind/Full
                                                          :incremental TextDocumentSyncKind/Incremental
                                                          TextDocumentSyncKind/Full))
                                            (.setSave (SaveOptions. true)))))

(s/def ::server-capabilities (s/and (s/keys :opt-un [::document-highlight-provider
                                                     ::signature-help-provider ::text-document-sync ::execute-command-provider ::completion-provider ::code-action-provider ::semantic-tokens-provider])
                                    (s/conformer #(doto (ServerCapabilities.)
                                                    (.setDocumentHighlightProvider ^Boolean (:document-highlight-provider %1))
                                                    (.setHoverProvider ^Boolean (:hover-provider %1))
                                                    (.setDeclarationProvider ^Boolean (:declaration-provider %1))
                                                    (.setImplementationProvider ^Boolean (:implementation-provider %1))
                                                    (.setSignatureHelpProvider (:signature-help-provider %1))
                                                    (.setCallHierarchyProvider ^Boolean (:call-hierarchy-provider %1))
                                                    (.setLinkedEditingRangeProvider ^Boolean (:linked-editing-range-provider %1))
                                                    (.setCodeActionProvider ^CodeActionOptions (:code-action-provider %1))
                                                    (.setCodeLensProvider (CodeLensOptions. ^Boolean (:code-lens-provider %1)))
                                                    (.setReferencesProvider ^Boolean (:references-provider %1))
                                                    (.setRenameProvider (RenameOptions. ^Boolean (:rename-provider %1)))
                                                    (.setDefinitionProvider ^Boolean (:definition-provider %1))
                                                    (.setDocumentFormattingProvider ^Boolean (:document-formatting-provider %1))
                                                    (.setDocumentRangeFormattingProvider ^Boolean (:document-range-formatting-provider %1))
                                                    (.setDocumentSymbolProvider ^Boolean (:document-symbol-provider %1))
                                                    (.setWorkspaceSymbolProvider ^Boolean (:workspace-symbol-provider %1))
                                                    (.setSemanticTokensProvider (:semantic-tokens-provider %1))
                                                    (.setExecuteCommandProvider (:execute-command-provider %1))
                                                    (.setTextDocumentSync ^TextDocumentSyncOptions (:text-document-sync %1))
                                                    (.setCompletionProvider (:completion-provider %1))
                                                    (.setExperimental (:experimental %))))))

(defn conform-or-log [^ILSPLogger logger spec value]
  (when value
    (try
      (let [result (s/conform spec value)]
        (if (= :clojure.spec.alpha/invalid result)
          (logger/error logger (s/explain-data spec value))
          result))
      (catch Exception ex
        (if (instance? ResponseErrorException ex)
          (throw ex)
          (logger/error logger ex spec value))))))

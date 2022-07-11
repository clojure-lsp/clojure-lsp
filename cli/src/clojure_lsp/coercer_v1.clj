(ns clojure-lsp.coercer-v1
  ;; This ns is a candidate for going back to lsp4clj.
  (:require
   [clojure.set :as set]
   [clojure.spec.alpha :as s]
   [lsp4clj.json-rpc.messages :as lsp.messages]
   [lsp4clj.protocols.logger :as logger]))

(set! *warn-on-reflection* true)

(def file-change-type-enum {1 :created 2 :changed 3 :deleted})
(s/def :file-event.v1/type (s/and int?
                                  file-change-type-enum
                                  (s/conformer file-change-type-enum)))
(s/def ::file-event (s/keys :req-un [::uri :file-event.v1/type]))
(s/def :did-change-watched-files.v1/changes (s/coll-of ::file-event))
(s/def ::did-change-watched-files-params (s/keys :req-un [:did-change-watched-files.v1/changes]))

(s/def :error.v1/code (s/and (s/or :kw keyword? :int int?)
                             (s/conformer second)))
(s/def :error.v1/message string?)

(s/def ::error (s/keys :req-un [:error.v1/code :error.v1/message]
                       :opt-un [::data]))

(s/def ::response-error (s/and (s/keys :req-un [::error])
                               (s/conformer
                                 (fn [{:keys [error]}]
                                   (lsp.messages/error-response (:code error) (:message error) (:data error))))))

(s/def ::line (s/and integer? (s/conformer int)))
(s/def ::character (s/and integer? (s/conformer int)))
(s/def ::position (s/keys :req-un [::line ::character]))
(s/def ::start ::position)
(s/def ::end ::position)
(s/def ::range (s/keys :req-un [::start ::end]))
(s/def ::selection-range ::range)

(def completion-kind-enum
  {:text 1 :method 2 :function 3 :constructor 4 :field 5 :variable 6 :class 7 :interface 8 :module 9
   :property 10 :unit 11 :value 12 :enum 13 :keyword 14 :snippet 15 :color 16 :file 17 :reference 18
   :folder 19 :enummember 20 :constant 21 :struct 22 :event 23 :operator 24 :typeparameter 25})

(s/def :completion-item.v1/kind (s/and keyword?
                                       completion-kind-enum
                                       (s/conformer completion-kind-enum)))

(def insert-text-format-enum
  {:plaintext 1
   :snippet 2})

(s/def :completion-item.v1/insert-text-format
  (s/and keyword?
         insert-text-format-enum
         (s/conformer insert-text-format-enum)))

(s/def ::new-text string?)
(s/def ::text-edit (s/keys :req-un [::new-text ::range]))
(s/def ::additional-text-edits (s/coll-of ::text-edit))
(s/def ::documentation (s/and (s/or :string string?
                                    :markup-content ::markup-content)
                              (s/conformer second)))

(s/def :prepare-rename.v1/placeholder string?)
(s/def ::prepare-rename (s/keys :req-un [:prepare-rename.v1/placeholder ::range]))

(s/def ::prepare-rename-or-error
  (s/and (s/or :error ::response-error
               :range ::prepare-rename
               :start ::range)
         (s/conformer second)))

(s/def ::completion-item (s/keys :req-un [::label]
                                 :opt-un [::additional-text-edits ::filter-text ::detail ::text-edit
                                          :completion-item.v1/kind ::documentation ::data
                                          ::insert-text :completion-item.v1/insert-text-format]))

(s/def ::completion-items (s/coll-of ::completion-item))
(s/def ::version (s/and integer? (s/conformer int)))
(s/def ::uri string?)

(s/def ::edits (s/coll-of ::text-edit))
(s/def ::text-document (s/keys :req-un [::version ::uri]))
(s/def ::text-document-edit (s/keys :req-un [::text-document ::edits]))
(s/def ::changes (s/coll-of (s/tuple string? ::edits) :kind map?))

(s/def :create-file.v1/options (s/keys :opt-un [::overwrite ::ignore-if-exists]))

(s/def :create-file.v1/kind (s/and string?
                                   #(= % "create")))
(s/def ::create-file (s/keys :req-un [:create-file.v1/kind ::uri]
                             :opt-un [:create-file.v1/options]))
(s/def :rename-file.v1/kind (s/and string?
                                   #(= % "rename")))
(s/def :rename-file.v1/old-uri ::uri)
(s/def :rename-file.v1/new-uri ::uri)

(s/def ::rename-file (s/keys :req-un [:rename-file.v1/kind :rename-file.v1/old-uri :rename-file.v1/new-uri]))

(s/def ::document-changes-entry (s/or :create-file ::create-file
                                      :rename-file ::rename-file
                                      :text-document-edit ::text-document-edit))
(s/def ::document-changes (s/and (s/coll-of ::document-changes-entry)
                                 (s/conformer #(map second %))))

(s/def ::workspace-edit
  (s/keys :opt-un [::document-changes ::changes]))

(s/def ::workspace-edit-or-error
  (s/and (s/or :error ::response-error
               :changes ::workspace-edit
               :document-changes ::workspace-edit)
         (s/conformer second)))

(s/def :workspace-edit-params.v1/edit ::workspace-edit-or-error)

(s/def ::workspace-edit-params
  (s/keys :req-un [:workspace-edit-params.v1/edit]))

(s/def ::location (s/keys :req-un [::uri ::range]))
(s/def ::locations (s/coll-of ::location))

(s/def :signature-help.v1/documentation ::documentation)

(s/def :signature-help.v1/parameter (s/keys :req-un [::label]
                                            :opt-un [:signature-help.v1/documentation]))

(s/def :signature-help.v1/parameters (s/coll-of :signature-help.v1/parameter))

(s/def :signature-help.v1/signature-information (s/keys :req-un [::label]
                                                        :opt-un [:signature-help.v1/documentation
                                                                 :signature-help.v1/parameters
                                                                 :signature-help.v1/active-parameter]))

(s/def :signature-help.v1/signatures (s/coll-of :signature-help.v1/signature-information))

(s/def ::signature-help (s/keys :req-un [:signature-help.v1/signatures]
                                :opt-un [:signature-help.v1/active-signature
                                         :signature-help.v1/active-parameter]))

(def symbol-kind-enum
  {:file 1 :module 2 :namespace 3 :package 4 :class 5 :method 6 :property 7 :field 8 :constructor 9
   :enum 10 :interface 11 :function 12 :variable 13 :constant 14 :string 15 :number 16 :boolean 17
   :array 18 :object 19 :key 20 :null 21 :enum-member 22 :struct 23 :event 24 :operator 25
   :type-parameter 26})

(s/def :symbol.v1/kind (s/and keyword?
                              symbol-kind-enum
                              (s/conformer symbol-kind-enum)))

(s/def :document-symbol.v1/selection-range ::range)

(s/def :document-symbol.v1/detail string?)

(s/def ::document-symbol (s/keys :req-un [::name
                                          :symbol.v1/kind
                                          ::range
                                          :document-symbol.v1/selection-range]
                                 :opt-un [:document-symbol.v1/detail :document-symbol.v1/children]))

(s/def :document-symbol.v1/children (s/coll-of ::document-symbol))

(s/def ::document-symbols (s/coll-of ::document-symbol))

(s/def ::document-highlight (s/keys :req-un [::range]))

(s/def ::document-highlights (s/coll-of ::document-highlight))

(s/def ::symbol-information (s/keys :req-un [::name :symbol.v1/kind ::location]))

(s/def ::workspace-symbols (s/coll-of ::symbol-information))

(s/def ::severity integer?)

(s/def ::code (s/conformer name))

(s/def ::diagnostic (s/keys :req-un [::range ::message]
                            :opt-un [::severity ::code ::tag ::source ::message]))
(s/def ::diagnostics (s/coll-of ::diagnostic))
(s/def ::publish-diagnostics-params (s/keys :req-un [::uri ::diagnostics]))

(s/def ::marked-string (s/and (s/or :string string?
                                    :marked-string (s/keys :req-un [::language ::value]))
                              (s/conformer second)))

(s/def :markup.v1/kind #{"plaintext" "markdown"})
(s/def :markup.v1/value string?)
(s/def ::markup-content (s/keys :req-un [:markup.v1/kind :markup.v1/value]))

(s/def ::contents (s/and (s/or :marked-strings (s/coll-of ::marked-string)
                               :markup-content ::markup-content)
                         (s/conformer second)))

(s/def ::hover (s/keys :req-un [::contents]
                       :opt-un [::range]))

(s/def :command.v1/title string?)
(s/def :command.v1/command string?)
(s/def :command.v1/arguments (s/coll-of any?))

(s/def ::command (s/keys :req-un [:command.v1/title :command.v1/command]
                         :opt-un [:command.v1/arguments]))

(def show-message-type-enum
  {:error 1
   :warning 2
   :info 3
   :log 4})

(s/def :show-message.v1/type (s/and keyword?
                                    show-message-type-enum
                                    (s/conformer show-message-type-enum)))

(s/def :show-message.v1/message string?)

(s/def ::show-message (s/keys :req-un [:show-message.v1/type
                                       :show-message.v1/message]))

(s/def :show-message-request-action.v1/title string?)

(s/def :show-message-request.v1/action (s/keys :req-un [:show-message-request-action.v1/title]))

(s/def :show-message-request.v1/actions (s/coll-of :show-message-request.v1/action))

(s/def ::show-message-request (s/keys :req-un [:show-message.v1/type
                                               :show-message.v1/message]
                                      :opt-un [:show-message-request.v1/actions]))

;; (def work-done-progress-kind-enum
;;   {:begin WorkDoneProgressKind/begin
;;    :report WorkDoneProgressKind/report
;;    :end WorkDoneProgressKind/end})
;;
;; (s/def :work-done-progress/kind (s/and keyword?
;;                                        work-done-progress-kind-enum))
;;
;; (s/def ::work-done-progress (s/and (s/keys :req-un [:work-done-progress/kind])
;;                                    (s/conformer (fn [w]
;;                                                   (case (:kind w)
;;                                                     :begin (doto (WorkDoneProgressBegin.)
;;                                                              (.setTitle (:title w))
;;                                                              (.setCancellable (:cancelable w))
;;                                                              (.setMessage (:message w))
;;                                                              (.setPercentage (int (:percentage w))))
;;                                                     :report (doto (WorkDoneProgressReport.)
;;                                                               (.setCancellable (:cancelable w))
;;                                                               (.setMessage (:message w))
;;                                                               (.setPercentage (int (:percentage w))))
;;                                                     :end (doto (WorkDoneProgressEnd.)
;;                                                            (.setMessage (:message w))))))))
;;
;; (s/def :progress/token string?)
;;
;; (s/def :progress/value ::work-done-progress)
;;
;; (s/def ::notify-progress (s/and (s/keys :req-un [:progress/token
;;                                                  :progress/value])
;;                                 (s/conformer #(ProgressParams. (Either/forLeft ^String (:token %))
;;                                                                (Either/forLeft ^WorkDoneProgressNotification (:value %))))))
;;
(s/def ::show-document-request
  (s/and (s/keys :req-un [::uri ::range]
                 :opt-un [::take-focus])
         (s/conformer (fn [element]
                        (set/rename-keys element {:range :selection})))))

(s/def :code-action.v1/title string?)

(s/def :code-action.v1/edit ::workspace-edit-or-error)

(def code-action-kind
  {:quick-fix "quickfix"
   :refactor "refactor"
   :refactor-extract "refactor.extract"
   :refactor-inline "refactor.inline"
   :refactor-rewrite "refactor.rewrite"
   :source "source"
   :source-organize-imports "source.organizeImports"})

(s/def :code-action.v1/preferred boolean?)

(s/def :code-action.v1/kind (s/and (s/or :keyword (s/and keyword?
                                                         code-action-kind
                                                         (s/conformer code-action-kind))
                                         :string string?)
                                   (s/conformer second)))

(s/def ::code-action (s/keys :req-un [:code-action.v1/title]
                             :opt-un [:code-action.v1/kind
                                      ::diagnostics
                                      :code-action.v1/edit
                                      ::command
                                      :code-action.v1/preferred
                                      ::data]))

(s/def ::code-actions (s/coll-of ::code-action))

(s/def ::code-lens (s/keys :req-un [::range]
                           :opt-un [::command ::data]))

(s/def ::code-lenses (s/coll-of ::code-lens))

(s/def ::semantic-tokens (s/keys :req-un [::data]
                                 :opt-un [::result-id]))

(s/def ::call-hierarchy-item (s/keys :req-un [::name
                                              :symbol.v1/kind
                                              ::uri
                                              ::range
                                              ::selection-range]
                                     :opt-un [::tags ::detail ::data]))

(s/def ::call-hierarchy-items (s/coll-of ::call-hierarchy-item))

(s/def :call-hierarchy.v1/from-ranges (s/coll-of ::range))
(s/def :call-hierarchy.v1/from ::call-hierarchy-item)
(s/def :call-hierarchy.v1/to ::call-hierarchy-item)

(s/def ::call-hierarchy-incoming-call (s/keys :req-un [:call-hierarchy.v1/from :call-hierarchy.v1/from-ranges]))

(s/def ::call-hierarchy-outgoing-call (s/keys :req-un [:call-hierarchy.v1/to :call-hierarchy.v1/from-ranges]))

(s/def ::call-hierarchy-incoming-calls (s/coll-of ::call-hierarchy-incoming-call))
(s/def ::call-hierarchy-outgoing-calls (s/coll-of ::call-hierarchy-outgoing-call))

(s/def :linked-editing-range.v1/ranges (s/coll-of ::range))

(s/def ::linked-editing-ranges
  (s/keys :req-un [:linked-editing-range.v1/ranges]
          :opt-un [::word-pattern]))

(s/def ::linked-editing-ranges-or-error
  (s/and (s/or :error ::response-error
               :ranges ::linked-editing-ranges)
         (s/conformer second)))

(s/def :server-capabilities.v1/signature-help-provider
  (s/conformer #(cond (vector? %) {:trigger-characters %}
                      (map? %) %
                      :else {:trigger-characters %})))
(s/def :server-capabilities.v1/code-action-provider
  (s/conformer #(when (vector? %) {:code-action-kinds %})))
(s/def :server-capabilities.v1/execute-command-provider
  (s/conformer #(when (vector? %) {:commands %})))
(s/def :server-capabilities.v1/code-lens-provider
  (s/conformer (fn [element] {:resolve-provider element})))
(s/def :server-capabilities.v1/rename-provider
  (s/conformer (fn [element] {:prepare-provider element})))
(s/def :server-capabilities.v1/semantic-tokens-provider
  (s/conformer #(when (and (:token-types %)
                           (:token-modifiers %))
                  {:legend {:token-types (:token-types %)
                            :token-modifiers (:token-modifiers %)}
                   :range (:range %)
                   :full (boolean (get % :full false))})))
(def text-docyment-sync-kind
  {:none 0
   :full 1
   :incremental 2})
(s/def :server-capabilities.v1/text-document-sync
  (s/conformer (fn [element]
                 {:open-close true
                  :change (or (get text-docyment-sync-kind element)
                              (get text-docyment-sync-kind :full))
                  :save {:include-text true}})))

(s/def ::server-capabilities
  (s/keys :opt-un [:server-capabilities.v1/code-action-provider
                   :server-capabilities.v1/code-lens-provider
                   :server-capabilities.v1/execute-command-provider
                   :server-capabilities.v1/rename-provider
                   :server-capabilities.v1/semantic-tokens-provider
                   :server-capabilities.v1/signature-help-provider
                   :server-capabilities.v1/text-document-sync]))

(defn conform-or-log [spec value]
  (when value
    (try
      (let [result (s/conform spec value)]
        (if (= :clojure.spec.alpha/invalid result)
          (logger/error (s/explain-data spec value))
          result))
      (catch Exception ex
        (logger/error ex spec value)))))

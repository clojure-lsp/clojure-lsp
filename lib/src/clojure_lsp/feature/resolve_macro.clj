(ns clojure-lsp.feature.resolve-macro
  (:require
   [borkdude.rewrite-edn :as r]
   [clojure-lsp.feature.file-management :as f.file-management]
   [clojure-lsp.kondo :as lsp.kondo]
   [clojure-lsp.queries :as q]
   [clojure-lsp.refactor.edit :as edit]
   [clojure-lsp.shared :as shared]
   [clojure.java.io :as io]
   [lsp4clj.protocols.logger :as logger]
   [clojure-lsp.producer :as producer]
   [rewrite-clj.zip :as z]))

(set! *warn-on-reflection* true)

(def ^:private excluded-macros
  '{clojure.core *
    clojure.core.async *
    clojure.test [deftest is testing]
    clojure.test.check.generators [let]
    cljs.core.async *
    cljs.core.async.macros *
    cats.core [->= ->>=]
    schema.core [defschema]
    compojure.core [defroutes let-routes]})

(defn ^:private find-function-name-position [zloc]
  (some-> zloc
          edit/find-function-usage-name-loc
          z/node
          meta))

(defn find-full-macro-symbol-to-resolve [zloc uri db]
  (when-let [{macro-name-row :row macro-name-col :col} (find-function-name-position zloc)]
    (let [filename (shared/uri->filename uri)
          element (q/find-element-under-cursor db filename macro-name-row macro-name-col)]
      (when (:macro element)
        (let [excluded-vars (get excluded-macros (:to element))]
          (when (and (not= excluded-vars '*)
                     (not (contains? excluded-vars (:name element))))
            (symbol (-> element :to name) (-> element :name name))))))))

(defn ^:private update-macro-resolve-for-config
  [resolved-full-symbol full-symbol config-loc]
  (-> config-loc
      z/node
      (r/assoc-in [:lint-as full-symbol] resolved-full-symbol)
      (str "\n")))

(defn ^:private resolve-macro-as [zloc uri resolved-full-symbol-str kondo-config-path db]
  (when-let [full-symbol (find-full-macro-symbol-to-resolve zloc uri db)]
    (let [kondo-config-file (io/file kondo-config-path)]
      (if (shared/file-exists? kondo-config-file)
        (->> (z/of-file kondo-config-file)
             (update-macro-resolve-for-config (symbol resolved-full-symbol-str) full-symbol))
        (->> (z/of-string "{}")
             (update-macro-resolve-for-config (symbol resolved-full-symbol-str) full-symbol))))))

(def ^:private known-full-symbol-resolve
  ["clojure.core/def"
   "clojure.core/defn"
   "clojure.core/let"
   "clojure.core/for"
   "clojure.core/->"
   "clojure.core/->>"
   "clj-kondo.lint-as/def-catch-all"])

(defn show-message-request-or-error [producer message error]
  (if-let [result (apply producer/show-message-request producer message)]
    result
    (do
      (logger/error error)
      nil)))

(defn resolve-macro-as!
  [zloc uri db {:keys [producer] :as components}]
  (let [project-root-uri (:project-root-uri db)]
    (when-let [resolved-full-symbol-str
               (show-message-request-or-error
                 producer
                 ["Select how LSP should resolve this macro:" :info (mapv #(hash-map :title %) known-full-symbol-resolve)]
                 "No response from client on how to resolve macro.")]
      (let [kondo-config-paths-options [(lsp.kondo/project-config-path project-root-uri)
                                        (lsp.kondo/home-config-path)]]
        (when-let [kondo-config-path (show-message-request-or-error
                                       producer
                                       ["Select where LSP should save this setting:" :info (mapv #(hash-map :title %) kondo-config-paths-options)]
                                       "No response from client on where to save setting.")]
          (if-let [new-kondo-config (resolve-macro-as zloc uri resolved-full-symbol-str kondo-config-path db)]
            (let [document (get-in db [:documents uri])]
              (io/make-parents kondo-config-path)
              (spit kondo-config-path new-kondo-config)
              (f.file-management/analyze-changes {:uri uri
                                                  :version (:v document)
                                                  :text (:text document)}
                                                 components)
              (logger/info (format "Resolving macro as %s. Saving setting into %s" resolved-full-symbol-str kondo-config-path)))
            (do
              (logger/error (format "Could not resolve macro at cursor to be resolved as '%s' for path '%s'" resolved-full-symbol-str kondo-config-path))
              (producer/show-message producer (format "No macro was found at cursor to resolve as '%s'." resolved-full-symbol-str) :error nil))))))
    {:no-op? true}))

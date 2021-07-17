(ns clojure-lsp.feature.resolve-macro
  (:require
   [borkdude.rewrite-edn :as r]
   [clojure-lsp.db :as db]
   [clojure-lsp.feature.file-management :as f.file-management]
   [clojure-lsp.parser :as parser]
   [clojure-lsp.producer :as producer]
   [clojure-lsp.queries :as q]
   [clojure-lsp.refactor.edit :as edit]
   [clojure-lsp.shared :as shared]
   [clojure.java.io :as io]
   [rewrite-clj.zip :as z]
   [taoensso.timbre :as log]))

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

(defn ^:private find-function-name-position [uri row col]
  (some-> (get-in @db/db [:documents uri :text])
          (parser/loc-at-pos row col)
          edit/find-function-usage-name-loc
          z/node
          meta))

(defn find-full-macro-symbol-to-resolve [uri row col]
  (when-let [{macro-name-row :row macro-name-col :col} (find-function-name-position uri row col)]
    (let [filename (shared/uri->filename uri)
          analysis (:analysis @db/db)
          element (q/find-element-under-cursor analysis filename macro-name-row macro-name-col)
          definition (q/find-definition analysis element)]
      (when (:macro definition)
        (let [excluded-vars (get excluded-macros (:ns definition))]
          (when (and (not= excluded-vars '*)
                     (not (contains? excluded-vars (:name definition))))
            (symbol (-> definition :ns name) (-> definition :name name))))))))

(defn ^:private update-macro-resolve-for-config
  [resolved-full-symbol full-symbol config-loc]
  (-> config-loc
      z/node
      (r/assoc-in [:lint-as full-symbol] resolved-full-symbol)
      (str "\n")))

(defn ^:private resolve-macro-as [uri row col resolved-full-symbol-str kondo-config-path]
  (when-let [full-symbol (find-full-macro-symbol-to-resolve uri row col)]
    (let [kondo-config-file (io/file kondo-config-path)]
      (if (.exists ^java.io.File kondo-config-file)
        (->> (z/of-file kondo-config-file)
             (update-macro-resolve-for-config (symbol resolved-full-symbol-str) full-symbol))
        (->> (z/of-string "{}")
             (update-macro-resolve-for-config (symbol resolved-full-symbol-str) full-symbol))))))

(defn resolve-macro-as!
  [uri row col resolved-full-symbol-str kondo-config-path]
  (if-let [new-kondo-config (resolve-macro-as uri row col resolved-full-symbol-str kondo-config-path)]
    (let [document (get-in @db/db [:documents uri])]
      (io/make-parents kondo-config-path)
      (spit kondo-config-path new-kondo-config)
      (f.file-management/analyze-changes {:uri uri
                                          :version (:v document)
                                          :text (:text document)})
      (log/info (format "Resolving macro as %s. Saving setting into %s" resolved-full-symbol-str kondo-config-path)))
    (do
      (log/error (format "Could not resolve macro at cursor to be resolved as '%s' for path '%s'" resolved-full-symbol-str kondo-config-path))
      (producer/window-show-message (format "No macro was found at cursor to resolve as '%s'." resolved-full-symbol-str) :error))))

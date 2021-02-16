(ns clojure-lsp.repl
  (:require
   [clojure-lsp.crawler :as crawler]
   [clojure-lsp.db :as db]
   [clojure-lsp.interop :as interop]
   [clojure-lsp.queries :as q]
   [clojure-lsp.shared :as shared]))


(def client-capabilities
  {})

(def project-root (shared/filename->uri (System/getProperty "user.dir")))

(defn ^:private initialize [project-root settings]
  (crawler/initialize-project project-root client-capabilities (interop/clean-client-settings settings)))

(defn find-references
  "Find all references of `var` on the existing session"
  [variable {:keys [settings]}]
  (when-let [{:keys [line column ns]} (meta (find-var variable))]
    (initialize project-root settings)
    (let [column (+ column 13) ; TODO get correct column, for some reason meta's column is always 1
          filename (str ns) ;TODO get absolute filename from ns
          ]
      (q/find-references-from-cursor (:analysis @db/db) filename line column false))))


(count (find-references 'clojure-lsp.shared/keywordize-first-depth {}))

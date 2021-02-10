(ns clojure-lsp.config
  (:require
    [clojure-lsp.shared :as shared]
    [clojure.string :as string]))

(defn kondo-for-paths [paths]
  {:cache true
   :parallel true
   :lint [(string/join (System/getProperty "path.separator") paths)]
   :config {:output {:analysis {:arglists true
                                :locals false
                                :keywords true}
                     :canonical-paths true}}})

(defn kondo-for-single-file [uri]
  {:cache false
   :lint ["-"]
   :lang (shared/uri->file-type uri)
   :filename (shared/uri->filename uri)
   :config {:output {:analysis {:arglists true
                                :locals true
                                :keywords true}
                     :canonical-paths true}}})

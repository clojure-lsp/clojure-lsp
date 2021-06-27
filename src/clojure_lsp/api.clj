(ns clojure-lsp.api
  (:require
   [clojure-lsp.internal-api :as internal-api])
  (:import
   [java.io File]))

(defn clean-ns!
  "Organize ns form, removing unused requires/refers/imports and sorting
  alphabetically.
  Options should be passed as keyword arguments and it accepts:

  `:project-root` a java.io.File representing the project root.

  `:namespaces` a coll of symbols representing the namespaces which should be cleaned,
  if empty all project namespaces will be considered."
  [& {:keys [project-root] :as options}]
  {:pre [(instance? File project-root)
         (.exists project-root)]}
  (internal-api/clean-ns! options))

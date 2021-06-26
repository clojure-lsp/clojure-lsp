(ns clojure-lsp.api
  (:require
    [clojure-lsp.internal-api :as internal-api]))

(defn clean-ns!
  "Organize ns form, removing unused requires/refers/imports and sorting
  alphabetically.
  Options should be passed as keyword arguments and it accepts:

  `:project-root` a string representing the path to the project root.

  `:namespaces` a coll of symbols representing the namespaces which should be cleaned,
  if empty all project namespaces will be considered."
  [& {:as options}]
  (internal-api/clean-ns! options))

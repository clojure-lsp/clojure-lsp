(ns clojure-lsp.queries
  (:refer-clojure :exclude [find-ns]))

(defn find-ns [usages]
  (->> usages
       (filter (comp :ns :tags))
       (some :sym)))

(ns clojure-lsp.feature.references
  (:require
   [clojure-lsp.db :as db]
   [clojure-lsp.shared :as shared]
   [clojure.set :as set]))

(defn find-under-cursor [line column env file-type]
  (let [file-types (if (= :cljc file-type)
                     #{:clj :cljs}
                     #{file-type})]
    (-> (->> env
             (filter (comp #(set/subset? % file-types) :file-type))
             (filter (comp #{:within} (partial shared/check-bounds line column)))
           ;; Pushes keywords last
             (sort-by (comp keyword? :sym)))
        (nth 0 nil))))

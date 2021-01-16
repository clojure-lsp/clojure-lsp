(ns clojure-lsp.feature.references
  (:require
   [clojure-lsp.db :as db]
   [clojure-lsp.feature.diagnostics :as f.diagnostic]
   [clojure-lsp.parser :as parser]
   [clojure-lsp.shared :as shared]
   [clojure.core.async :as async]
   [clojure.set :as set]
   [clojure.tools.logging :as log]))

(defn find-after-cursor [line column env file-type]
  (let [file-types (if (= :cljc file-type)
                     #{:clj :cljs}
                     #{file-type})]
    (->> env
         (filter (comp #(set/subset? % file-types) :file-type))
         (partition-all 3 1)
         (filter (comp #{:within :before} (partial shared/check-bounds line column) first))
         first)))

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

(defn reference-usages [doc-id row col]
  (let [file-envs (:file-envs @db/db)
        local-env (get file-envs doc-id)
        cursor (find-under-cursor row col local-env (shared/uri->file-type doc-id))]
    (into (for [[uri usages] (:file-envs @db/db)
                {:keys [sym tags] :as usage} usages
                :when (and (= sym (:sym cursor))
                           (not (contains? tags :declare)))]
            {:uri uri
             :usage usage}))))

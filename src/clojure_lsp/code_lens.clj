(ns clojure-lsp.code-lens
  (:require
    [clojure-lsp.db :as db]
    [clojure-lsp.queries :as q]
    [clojure-lsp.shared :as shared]))

(defn reference-code-lens [uri]
  (let [analysis (get @db/db :analysis)]
    (->> (q/find-vars analysis (shared/uri->filename uri) true)
         (filter #(not= 'clojure.test/deftest (:defined-by %)))
         (map (fn [var]
                {:range (shared/->range var)
                 :data  [uri (:name-row var) (:name-col var)]})))))

(defn resolve-code-lens [uri row col range]
  (let [filename (shared/uri->filename uri)
        references (q/find-references-from-cursor (:analysis @db/db) filename row col false)]
    {:range range
     :command {:title (-> references count (str " references"))
               :command "code-lens-references"
               :arguments [uri row col]}}))

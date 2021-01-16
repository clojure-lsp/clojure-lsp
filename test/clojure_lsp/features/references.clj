(ns clojure-lsp.features.references
  (:require
    [clojure-lsp.db :as db]
    [clojure-lsp.feature.references :as f.references]
    [clojure-lsp.parser :as parser]
    [clojure.test :refer [deftest is]]))

(deftest reference-usages-test
  (let [code "(let [something 1] something something)"
        usages (parser/find-usages code :clj {})]
    (reset! db/db {:documents {"file://a.clj" {:text code}}
                   :file-envs {"file://a.clj" usages}})
    (let [references (f.references/reference-usages "file://a.clj" 1 7)]
      (is (= 2 (count references))))))

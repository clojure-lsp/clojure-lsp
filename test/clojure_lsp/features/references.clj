(ns clojure-lsp.features.references
  (:require
    [clojure-lsp.db :as db]
    [clojure-lsp.feature.references :as f.references]
    [clojure-lsp.parser :as parser]
    [clojure.test :refer [deftest testing is]]))

(deftest reference-usages-test
  (testing "not including declaration"
    (let [code "(let [something 1] something something)"
          usages (parser/find-usages code :clj {})]
      (reset! db/db {:documents {"file://a.clj" {:text code}}
                     :file-envs {"file://a.clj" usages}})
      (let [references (f.references/reference-usages "file://a.clj" 1 7 false)]
        (is (= 2 (count references))))))
  (testing "including declaration"
    (let [code "(let [something 1] something something)"
          usages (parser/find-usages code :clj {})]
      (reset! db/db {:documents {"file://a.clj" {:text code}}
                     :file-envs {"file://a.clj" usages}})
      (let [references (f.references/reference-usages "file://a.clj" 1 7 true)]
        (is (= 3 (count references)))
        (is (= "something"
               (first (map (comp :str :usage) references))))))))

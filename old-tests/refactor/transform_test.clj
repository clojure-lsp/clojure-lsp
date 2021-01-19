(ns clojure-lsp.refactor.transform-test
  (:require
    [clojure-lsp.db :as db]
    [clojure-lsp.feature.references :as f.references]
    [clojure-lsp.parser :as parser]
    [clojure-lsp.refactor.edit :as edit]
    [clojure-lsp.refactor.transform :as transform]
    [clojure.string :as string]
    [clojure.test :refer :all]
    [rewrite-clj.zip :as z]
    [clojure-lsp.test-helper :as h]
    [clojure-lsp.queries :as q]
    [clojure-lsp.shared :as shared]))

(defn code [& strings] (string/join "\n" strings))

(deftest inline-symbol
  (testing "simple let"
    (h/load-code-and-locs "(let [something 1] something something)")
    (let [results (transform/inline-symbol "file:///a.clj" 1 7)
          a-results (get results "file:///a.clj")]
      (is (map? results))
      (is (= 1 (count results)))
      (is (= 3 (count a-results)))
      (is (= [nil "1" "1"] (map (comp z/string :loc) a-results)))))
  #_(testing "def in another file"
      (let [a-code "(ns a) (def something (1 * 60))"
            b-code "(ns b (:require a)) (inc a/something)"
            a-usages (parser/find-usages a-code :clj {})
            b-usages (parser/find-usages b-code :clj {})]
        (reset! db/db {:documents {"file:///a.clj" {:text a-code}
                                   "file:///b.clj" {:text b-code}}
                       :file-envs {"file:///a.clj" a-usages
                                   "file:///b.clj" b-usages}})
        (let [zloc (z/find-value (z/of-string b-code) z/next 'a/something)
              pos (meta (z/node zloc))
              definition (f.definition/definition-usage "file:///b.clj" (:row pos) (:col pos))
              references (f.references/reference-usages "file:///b.clj" (:row pos) (:col pos))
              results (transform/inline-symbol definition references)
              a-results (get results "file:///a.clj")
              b-results (get results "file:///b.clj")]
          (is (map? results))
          (is (= 2 (count results)))
          (is (= [nil] (map (comp z/string :loc) a-results)))
          (is (= ["(1 * 60)"] (map (comp z/string :loc) b-results)))))))

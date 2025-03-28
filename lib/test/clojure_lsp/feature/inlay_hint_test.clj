(ns clojure-lsp.feature.inlay-hint-test
  (:require
   [clojure-lsp.feature.inlay-hint :as f.inlay-hint]
   [clojure-lsp.test-helper :as h]
   [clojure.test :refer [deftest testing]]))

(deftest inlay-hint-test
  #_(testing "built-in functions"
    (h/load-code-and-locs (h/code "(defn foo [bar] bar)"
                                  "(foo 1 2)"))
    (h/assert-submaps
      [{:position {:line 0 :character 2}
        :label "x:"
        :kind :parameter
        :padding-right true}
       {:position {:line 0 :character 4}
        :label "y:"
        :kind :parameter
        :padding-right true}]
      (f.inlay-hint/inlay-hint h/default-uri (h/db))))
  (testing "user functions"
      (h/load-code-and-locs (h/code "(defn my-fn [foo]"
                                    " foo)"
                                    "(my-fn 1)"))
      (h/assert-submaps
        [{:position {:line 2 :character 7}
          :label "foo:"
          :kind :parameter
          :padding-right true}]
        (f.inlay-hint/inlay-hint h/default-uri (h/db)))))

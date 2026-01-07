(ns clojure-lsp.feature.replace-refer-all-test
  (:require
   [clojure-lsp.feature.replace-refer-all :as f.replace-refer-all]
   [clojure-lsp.test-helper.internal :as h]
   [clojure.test :refer [deftest is]]))

(h/reset-components-before-test)

(deftest replace-with-refers-test
  (let [zloc (h/load-code-and-zloc (h/code "(ns foo"
                                           " (:require "
                                           "  [other-ns :refer :all]"
                                           "  [my.cool-ns :refer |:all]))"))
        result (f.replace-refer-all/replace-with-refers
                 zloc
                 ["some" "cool"])]
    (is (= (h/code "(ns foo"
                   " (:require "
                   "  [other-ns :refer :all]"
                   "  [my.cool-ns :refer [some cool]]))")
           (h/changes->code result (h/db))))))

(deftest replace-with-alias-test
  (let [zloc (h/load-code-and-zloc (h/code "(ns foo"
                                           " (:require "
                                           "  [other-ns :refer :all]"
                                           "  [clojure.string :refer |:all]))"
                                           "(join [] \"\")"
                                           "(join [] \"\")"))
        result (f.replace-refer-all/replace-with-alias
                 zloc
                 h/default-uri
                 (h/db))]
    (is (= (h/code "(ns foo"
                   " (:require "
                   "  [other-ns :refer :all]"
                   "  [clojure.string :as an-alias]))"
                   "(an-alias/join [] \"\")"
                   "(an-alias/join [] \"\")")
           (h/changes->code result (h/db))))))

(deftest replace-with-alias-does-not-mangle-forms-test
  (h/load-code (h/code "(ns y)"
                       "(defn y-f [& a] a)"
                       "(def y-d \"a\")")
               (h/file-uri "file:///y.clj"))
  (let [zloc (h/load-code-and-zloc (h/code "(ns x"
                                           " (:require"
                                           "  [y :refer |:all]))"
                                           "(update-in {} [:a :b] y-f 1)"
                                           "(merge {:a y-d :b y-d :c y-d} {:a 1} {:a 2})"))
        result (f.replace-refer-all/replace-with-alias
                 zloc
                 h/default-uri
                 (h/db))]
    (is (= (h/code "(ns x"
                   " (:require"
                   "  [y :as an-alias]))"
                   "(update-in {} [:a :b] an-alias/y-f 1)"
                   "(merge {:a an-alias/y-d :b an-alias/y-d :c an-alias/y-d} {:a 1} {:a 2})")
           (h/changes->code result (h/db))))))

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

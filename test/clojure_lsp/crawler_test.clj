(ns clojure-lsp.crawler-test
  (:require [clojure-lsp.crawler :as crawler]
            [clojure.test :refer :all]))

(def unused-alias-code
  "(ns foo.bar
     (:require [foo.baz :as baz]
               [foo.bah :as bah]))
   (def zas (bah/some))")

(deftest find-unused-aliases-test
  (testing "When there is unused aliases"
    (with-redefs [slurp (constantly unused-alias-code)]
      (is (= #{'foo.baz}
             (crawler/find-unused-aliases "file://a.clj"))))))

(def unused-refer-code
  "(ns foo.bar
     (:require [foo.baz :refer [other]]
               [foo.bah :as bah]))
   (def zas (bah/some))")

(def unused-refer-with-used-refers-on-other-require-code
  "(ns foo.bar
     (:require [foo.baz :refer [other]]
               [foo.bao :refer [another]]
               [foo.bah :as bah]))
   (def zas
     (bah/some)
     (another))")

(def unused-refer-with-used-refers-on-same-require-code
  "(ns foo.bar
     (:require [foo.baz :refer [other another]]
               [foo.bah :as bah]))
   (def zas
     (bah/some)
     (another))")

(deftest find-unused-refers-test
  (testing "When there is a unused refer"
    (with-redefs [slurp (constantly unused-refer-code)]
      (is (= #{'foo.baz}
             (crawler/find-unused-refers "file://a.clj")))))
  (testing "When there is a unused refer but used refers on other require"
    (with-redefs [slurp (constantly unused-refer-with-used-refers-on-other-require-code)]
      (is (= #{'foo.baz}
             (crawler/find-unused-refers "file://a.clj")))))
  (testing "When there is a unused refer but used refers on same require"
    (with-redefs [slurp (constantly unused-refer-with-used-refers-on-same-require-code)]
      (is (= #{}
             (crawler/find-unused-refers "file://a.clj"))))))

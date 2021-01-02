(ns clojure-lsp.crawler-test
  (:require
   [clojure-lsp.crawler :as crawler]
   [clojure-lsp.feature.references :as f.references]
   [clojure.test :refer :all]))

(def unused-alias-code
  "(ns foo.bar
     (:require [foo.baz :as baz]
               [foo.bah :as bah]))
   (def zas (bah/some))")

(deftest find-unused-aliases-test
  (testing "When there is unused aliases"
    (let [usages (f.references/safe-find-references "file://a.clj" unused-alias-code false false)]
      (is (= #{'foo.baz}
             (crawler/find-unused-aliases usages))))))

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
     (:require [foo.baz :refer [other another some-other]]
               [foo.bah :as bah]))
   (def zas
     (bah/some)
     (another))")

(deftest find-unused-refers-test
  (testing "When there is a unused refer"
    (let [usages (f.references/safe-find-references  "file://a.clj" unused-refer-code false false)]
      (is (= #{'foo.baz/other}
             (crawler/find-unused-refers usages)))))
  (testing "When there is a unused refer but used refers on other require"
    (let [usages (f.references/safe-find-references  "file://a.clj" unused-refer-with-used-refers-on-other-require-code false false)]
      (is (= #{'foo.baz/other}
             (crawler/find-unused-refers usages)))))
  (testing "When there is a unused refer but used refers on same require"
    (let [usages (f.references/safe-find-references  "file://a.clj" unused-refer-with-used-refers-on-same-require-code false false)]
      (is (= #{'foo.baz/other 'foo.baz/some-other}
             (crawler/find-unused-refers usages))))))

(ns clojure-lsp.crawler-test
  (:require
    [clojure-lsp.crawler :as crawler]
    [clojure-lsp.db :as db]
    [clojure-lsp.feature.references :as f.references]
    [clojure.string :as string]
    [clojure.test :refer :all]))

(defn code [& strings] (string/join "\n" strings))

#_
(deftest find-unused-aliases-test
  (testing "When there is unused aliases"
    (let [code (code "(ns foo.bar"
                     "  (:require [foo.baz :as baz]"
                     "  [foo.bah :as bah]))"
                     "(def zas (bah/some))")
          usages (f.references/safe-find-references "file://a.clj" code false false)]
      (is (= #{'foo.baz}
             (crawler/find-unused-aliases usages))))))

#_
(deftest find-unused-refers-test
  (testing "When there is a unused refer"
    (let [code (code "(ns foo.bar"
                     "  (:require [foo.baz :refer [other]]"
                     "            [foo.bah :as bah]))"
                     "(def zas (bah/some))")
          usages (f.references/safe-find-references  "file://a.clj" code false false)]
      (is (= #{'foo.baz/other}
             (crawler/find-unused-refers usages)))))
  (testing "When there is a unused refer but used refers on other require"
    (let [code (code "(ns foo.bar"
                     "  (:require [foo.baz :refer [other]]"
                     "            [foo.bao :refer [another]]"
                     "            [foo.bah :as bah]))"
                     "(def zas"
                     "(bah/some)"
                     "(another))")
          usages (f.references/safe-find-references  "file://a.clj" code false false)]
      (is (= #{'foo.baz/other}
             (crawler/find-unused-refers usages)))))
  (testing "When there is a unused refer but used refers on same require"
    (let [code (code "(ns foo.bar"
                     "  (:require [foo.baz :refer [other another some-other]]"
                     "            [foo.bah :as bah]))"
                     "(def zas"
                     "(bah/some)"
                     "(another))")
          usages (f.references/safe-find-references  "file://a.clj" code false false)]
      (is (= #{'foo.baz/other 'foo.baz/some-other}
             (crawler/find-unused-refers usages))))))

#_
(deftest find-unused-imports-test
  (testing "When there is a used full import"
    (reset! db/db {})
    (let [code (code "(ns foo.bar"
                     "  (:import java.util.Date))"
                     "Date.")
          usages (f.references/safe-find-references  "file://a.clj" code false false)]
      (is (= #{}
             (crawler/find-unused-imports usages)))))
  (testing "When there is a used import using method"
    (reset! db/db {})
    (let [code (code "(ns foo.bar"
                     "  (:import java.util.Date))"
                     "Date/Foo")
          usages (f.references/safe-find-references  "file://a.clj" code false false)]
      (is (= #{}
             (crawler/find-unused-imports usages)))))
  (testing "When there is a unused full import"
    (reset! db/db {})
    (let [code (code "(ns foo.bar"
                     "  (:import java.util.Date))")
          usages (f.references/safe-find-references  "file://a.clj" code false false)]
      (is (= #{'java.util.Date}
             (crawler/find-unused-imports usages)))))
  (testing "When there is a unused full import on exclude settings"
    (reset! db/db {:settings {:linters {:unused-import {:exclude '[java.util.Date]}}}})
    (let [code (code "(ns foo.bar"
                     "  (:import java.util.Date))"
                     "Date.")
          usages (f.references/safe-find-references  "file://a.clj" code false false)]
      (is (= #{}
             (crawler/find-unused-imports usages)))))
  (testing "When there is a unused and used full import"
    (reset! db/db {})
    (let [code (code "(ns foo.bar"
                     "  (:import java.util.Date java.util.Calendar))"
                     "Date.")
          usages (f.references/safe-find-references  "file://a.clj" code false false)]
      (is (= #{'java.util.Calendar}
             (crawler/find-unused-imports usages)))))
  (testing "When there is multiple unused imports"
    (reset! db/db {})
    (let [code (code "(ns foo.bar"
                     "  (:import java.time.LocalDate java.time.LocalDateTime"
                     "           [java.util Date"
                     "                      Calendar]))"
                     "LocalDate.")
          usages (f.references/safe-find-references  "file://a.clj" code false false)]
      (is (= '#{java.util.Date java.util.Calendar java.time.LocalDateTime}
             (crawler/find-unused-imports usages))))))

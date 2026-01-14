(ns clojure-lsp.settings-test
  (:require
   [clojure-lsp.settings :as settings]
   [clojure-lsp.shared :as shared]
   [clojure-lsp.test-helper.internal :as h]
   [clojure.test :refer [deftest is]]
   [rewrite-clj.zip :as z]))

(h/reset-components-before-test)

(deftest kwd-string-test
  (is (= :foo (settings/kwd-string :foo)))
  (is (= :foo (settings/kwd-string "foo")))
  (is (= :foo (settings/kwd-string ":foo")))
  (is (nil? (settings/kwd-string 'foo)))
  (is (nil? (settings/kwd-string 1)))
  (is (nil? (settings/kwd-string [])))
  (is (nil? (settings/kwd-string nil))))

(deftest parse-source-paths-test
  (is (nil? (settings/parse-source-paths [])))
  (is (nil? (settings/parse-source-paths [:foo])))
  (is (= #{"foo"} (settings/parse-source-paths ["foo" :bar])))
  (doseq [f ["foo" ":foo"]
          b ["bar" ":bar"]]
    (is (= #{"foo" "bar"} (settings/parse-source-paths [f b]))))
  (doseq [b [:bar 'bar 1 [] nil]]
    (is (= #{"foo"} (settings/parse-source-paths ["foo" b])))))

(deftest parse-source-aliases-test
  (is (nil? (settings/parse-source-aliases [])))
  (is (nil? (settings/parse-source-aliases ['bar])))
  (doseq [f [:foo "foo" ":foo"]
          b [:bar "bar" ":bar"]]
    (is (= #{:foo :bar} (settings/parse-source-aliases [f b]))))
  (doseq [b ['bar 1 [] nil]]
    (is (= #{:foo} (settings/parse-source-aliases [:foo b])))))

(deftest all-test
  (swap! (h/db*) shared/deep-merge {:settings {:a {:b {:c 2}}}})
  (is (= {:a {:b {:c 2}}}
         (settings/all (h/db)))))

(deftest legacy-linsters-kondo-config-test
  (swap! (h/db*) shared/deep-merge {:settings {:a {:b {:c 2}}}
                                    :kondo-config {:bar :a
                                                   :config-in-ns {:a 1}
                                                   :ns-groups {:b 2}
                                                   :linters {:foo :a
                                                             :clojure-lsp/unused-public-var {:level :error}}}})
  (is (= {:a {:b {:c 2}}
          :config-in-ns {:a 1}
          :ns-groups {:b 2}
          :linters {:clojure-lsp/unused-public-var {:level :error}}}
         (settings/all (h/db)))))

(deftest get-test
  (swap! (h/db*) shared/deep-merge {:settings {:a {:b {:c 2}}}})
  (is (= 2 (settings/get (h/db) [:a :b :c])))
  (is (= {:c 2} (settings/get (h/db) [:a :b])))
  (is (= {:b {:c 2}} (settings/get (h/db) [:a])))
  (is (= 10 (settings/get (h/db) [:d] 10))))

(deftest clean-client-settings-test
  (let [raw-settings {:linters
                      {:clojure-lsp/unused-public-var
                       {:level "off"}}

                      :cljfmt
                      {:indentation? false}}
        cleaned (settings/clean-client-settings raw-settings)]
    (is (= {:level :off}
           (get-in cleaned [:linters :clojure-lsp/unused-public-var])))

    (is (= {:indentation? false}
           (:cljfmt cleaned)))))

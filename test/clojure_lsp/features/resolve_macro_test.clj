(ns clojure-lsp.features.resolve-macro-test
  (:require
   [clojure-lsp.db :as db]
   [clojure-lsp.feature.resolve-macro :as f.resolve-macro]
   [clojure-lsp.test-helper :as h]
   [clojure.core.async :as async]
   [clojure.test :refer [deftest is testing]]))

(h/reset-db-after-test)

(deftest find-full-macro-symbol-to-resolve
  (h/load-code-and-locs (h/code "(ns some-ns)"
                                "(defmacro foo [name & body] @body)"
                                "(foo my-fn)"
                                "(+ 1 2)"))
  (testing "inside a macro usage"
    (is (= 'some-ns/foo
           (f.resolve-macro/find-full-macro-symbol-to-resolve (h/file-uri "file:///a.clj") 3 7))))
  (testing "not inside a macro usage"
    (is (not (f.resolve-macro/find-full-macro-symbol-to-resolve (h/file-uri "file:///a.clj") 4 5)))))

(deftest resolve-macro-as
  (alter-var-root #'db/diagnostics-chan (constantly (async/chan 1)))
  (h/load-code-and-locs (h/code "(ns some-ns)"
                                "(defmacro foo [name] `(def ~name))"
                                "(foo my-fn)"
                                "(+ 1 2)"))
  (testing "resolving macro as def"
    (is (= "{:lint-as {some-ns/foo clojure.core/def}}\n"
           (#'f.resolve-macro/resolve-macro-as (h/file-uri "file:///a.clj") 3 7 "clojure.core/def" ".any-clj-kondo/config.edn")))))

(ns clojure-lsp.features.resolve-macro-test
  (:require
   [clojure-lsp.db :as db]
   [clojure-lsp.feature.resolve-macro :as f.resolve-macro]
   [clojure-lsp.test-helper :as h]
   [clojure.test :refer [deftest is testing]]))

(h/reset-db-after-test)

(deftest find-full-macro-symbol-to-resolve
  (let [a-code (h/code "(ns some-ns)"
                       "(defmacro foo [name & body] @body)"
                       "(foo |my-fn)"
                       "(+ 1 2)")
        b-code (h/code "(ns some-ns)"
                       "(defmacro foo [name & body] @body)"
                       "(foo my-fn)"
                       "(+ |1 2)")]
    (h/load-code-and-locs a-code (h/file-uri "file:///a.clj"))
    (h/load-code-and-locs b-code (h/file-uri "file:///b.clj"))
    (testing "inside a macro usage"
      (is (= 'some-ns/foo
             (f.resolve-macro/find-full-macro-symbol-to-resolve (h/zloc-from-code a-code) (h/file-uri "file:///a.clj") @db/db*))))
    (testing "not inside a macro usage"
      (is (not (f.resolve-macro/find-full-macro-symbol-to-resolve (h/zloc-from-code b-code) (h/file-uri "file:///a.clj") @db/db*))))))

(deftest resolve-macro-as
  (let [code (h/code "(ns some-ns)"
                     "(defmacro foo [name] `(def ~name))"
                     "(foo my|-fn)"
                     "(+ 1 2)")]
    (h/load-code-and-locs code)
    (testing "resolving macro as def"
      (is (= "{:lint-as {some-ns/foo clojure.core/def}}\n"
             (#'f.resolve-macro/resolve-macro-as (h/zloc-from-code code) (h/file-uri "file:///a.clj") "clojure.core/def" ".any-clj-kondo/config.edn" @db/db*))))))

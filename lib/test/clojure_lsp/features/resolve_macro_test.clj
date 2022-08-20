(ns clojure-lsp.features.resolve-macro-test
  (:require
   [clojure-lsp.feature.resolve-macro :as f.resolve-macro]
   [clojure-lsp.test-helper :as h]
   [clojure.test :refer [deftest is testing]]))

(deftest find-full-macro-symbol-to-resolve
  (let [components (h/make-components)
        a-code (h/code "(ns some-ns)"
                       "(defmacro foo [name & body] @body)"
                       "(foo |my-fn)"
                       "(+ 1 2)")
        b-code (h/code "(ns some-ns)"
                       "(defmacro foo [name & body] @body)"
                       "(foo my-fn)"
                       "(+ |1 2)")
        a-zloc (h/load-code-and-zloc a-code (h/file-uri "file:///a.clj") components)
        b-zloc (h/load-code-and-zloc b-code (h/file-uri "file:///b.clj") components)
        db (h/db components)]
    (testing "inside a macro usage"
      (is (= 'some-ns/foo
             (f.resolve-macro/find-full-macro-symbol-to-resolve a-zloc (h/file-uri "file:///a.clj") db))))
    (testing "not inside a macro usage"
      (is (not (f.resolve-macro/find-full-macro-symbol-to-resolve b-zloc (h/file-uri "file:///b.clj") db))))))

(deftest resolve-macro-as
  (let [components (h/make-components)
        code (h/code "(ns some-ns)"
                     "(defmacro foo [name] `(def ~name))"
                     "(foo my|-fn)"
                     "(+ 1 2)")
        zloc (h/load-code-and-zloc code h/default-uri components)
        db (h/db components)]
    (testing "resolving macro as def"
      (is (= "{:lint-as {some-ns/foo clojure.core/def}}\n"
             (#'f.resolve-macro/resolve-macro-as zloc (h/file-uri "file:///a.clj") "clojure.core/def" ".any-clj-kondo/config.edn" db))))))

(ns clojure-lsp.features.hover-test
  (:require
   [clojure-lsp.db :as db]
   [clojure-lsp.feature.hover :as f.hover]
   [clojure-lsp.test-helper :as h]
   [clojure.string :as string]
   [clojure.test :refer [deftest is testing]]))

(h/reset-db-after-test)

(defn ^:private join [coll]
  (string/join "\n" coll))

(deftest hover
  (let [start-code "```clojure"
        end-code "```"
        line-break "\n----\n"
        code (str "(ns a)\n"
                  "(defn foo \"Some cool docs :foo\" [x] x)\n"
                  "(defn bar [y] y)\n"
                  "(|foo 1)\n"
                  "(|bar 1)")
        [[foo-row foo-col]
         [bar-row bar-col]] (h/load-code-and-locs code)]
    (testing "with docs"
      (let [sym "a/foo"
            sig "[x]"
            doc "Some cool docs :foo"
            filename (h/file-path "/a.clj")]
        (testing "show-docs-arity-on-same-line? disabled"
          (testing "plain"
            (is (= [{:language "clojure" :value sym}
                    {:language "clojure" :value sig}
                    doc
                    filename]
                   (:contents (f.hover/hover (h/file-path "/a.clj") foo-row foo-col db/db)))))
          (testing "markdown"
            (swap! db/db merge {:client-capabilities {:text-document {:hover {:content-format ["markdown"]}}}})
            (is (= {:kind  "markdown"
                    :value (join [start-code sym sig end-code
                                  line-break
                                  doc
                                  line-break
                                  (str "*[" filename "](file:///a.clj)*")])}
                   (:contents (f.hover/hover (h/file-path "/a.clj") foo-row foo-col db/db))))))

        (testing "show-docs-arity-on-same-line? enabled"
          (testing "plain"
            (swap! db/db merge {:settings {:show-docs-arity-on-same-line? true} :client-capabilities nil})
            (is (= [{:language "clojure" :value (str sym " " sig)}
                    doc
                    filename]
                   (:contents (f.hover/hover (h/file-path "/a.clj") foo-row foo-col db/db)))))

          (testing "markdown"
            (swap! db/db merge {:client-capabilities {:text-document {:hover {:content-format ["markdown"]}}}})
            (is (= {:kind  "markdown"
                    :value (join [start-code (str sym " " sig) end-code
                                  line-break
                                  doc
                                  line-break
                                  (str "*[" filename "](file:///a.clj)*")])}
                   (:contents (f.hover/hover (h/file-path "/a.clj") foo-row foo-col db/db))))))

        (testing "hide-filename? enabled"
          (testing "plain"
            (swap! db/db merge {:settings {:hover {:hide-file-location? true}} :client-capabilities nil})
            (is (= [{:language "clojure" :value sym}
                    {:language "clojure" :value sig}
                    doc]
                   (:contents (f.hover/hover (h/file-path "/a.clj") foo-row foo-col db/db)))))
          (testing "markdown"
            (swap! db/db merge {:settings {:hover {:hide-file-location? true}} :client-capabilities {:text-document {:hover {:content-format ["markdown"]}}}})
            (is (= {:kind  "markdown"
                    :value (join [start-code sym sig end-code
                                  line-break
                                  doc])}
                   (:contents (f.hover/hover (h/file-path "/a.clj") foo-row foo-col db/db))))))))

    (testing "without docs"
      (let [sym "a/bar"
            sig "[y]"
            filename (h/file-path "/a.clj")]
        (testing "show-docs-arity-on-same-line? disabled"
          (testing "plain"
            (swap! db/db merge {:settings {:show-docs-arity-on-same-line? false} :client-capabilities nil})
            (is (= [{:language "clojure" :value sym}
                    {:language "clojure" :value sig}
                    filename]
                   (:contents (f.hover/hover (h/file-path "/a.clj") bar-row bar-col db/db)))))
          (testing "markdown"
            (swap! db/db merge {:client-capabilities {:text-document {:hover {:content-format ["markdown"]}}}})
            (is (= {:kind  "markdown"
                    :value (join [start-code sym sig end-code
                                  line-break
                                  (str "*[" filename "](file:///a.clj)*")])}
                   (:contents (f.hover/hover (h/file-path "/a.clj") bar-row bar-col db/db))))))

        (testing "show-docs-arity-on-same-line? enabled"
          (testing "plain"
            (swap! db/db merge {:settings {:show-docs-arity-on-same-line? true} :client-capabilities nil})
            (is (= [{:language "clojure" :value (str sym " " sig)}
                    filename]
                   (:contents (f.hover/hover (h/file-path "/a.clj") bar-row bar-col db/db)))))

          (testing "markdown"
            (swap! db/db merge {:client-capabilities {:text-document {:hover {:content-format ["markdown"]}}}})
            (is (= {:kind "markdown"
                    :value (join [start-code (str sym " " sig) end-code
                                  line-break
                                  (str "*[" filename "](file:///a.clj)*")])}
                   (:contents (f.hover/hover (h/file-path "/a.clj") bar-row bar-col db/db))))))))))

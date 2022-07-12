(ns clojure-lsp.features.hover-test
  (:require
   [clojure-lsp.db :as db]
   [clojure-lsp.feature.hover :as f.hover]
   [clojure-lsp.shared :as shared]
   [clojure-lsp.test-helper :as h]
   [clojure.string :as string]
   [clojure.test :refer [deftest is testing]]))

(h/reset-db-after-test)

(defn ^:private join [coll]
  (string/join "\n" coll))

(deftest hover
  (swap! db/db* shared/deep-merge {:settings {:hover {:clojuredocs false}}})
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
                   (:contents (f.hover/hover (h/file-uri "file:///a.clj") foo-row foo-col db/db*)))))
          (testing "markdown"
            (swap! db/db* shared/deep-merge {:client-capabilities {:text-document {:hover {:content-format ["markdown"]}}}})
            (is (= {:kind  "markdown"
                    :value (join [start-code sym sig end-code
                                  ""
                                  doc
                                  line-break
                                  (str "*[" filename "](file:///a.clj)*")])}
                   (:contents (f.hover/hover (h/file-uri "file:///a.clj") foo-row foo-col db/db*))))))

        (testing "show-docs-arity-on-same-line? enabled"
          (testing "plain"
            (swap! db/db* shared/deep-merge {:settings {:show-docs-arity-on-same-line? true}
                                             :client-capabilities nil})
            (is (= [{:language "clojure" :value (str sym " " sig)}
                    doc
                    filename]
                   (:contents (f.hover/hover (h/file-uri "file:///a.clj") foo-row foo-col db/db*)))))

          (testing "markdown"
            (swap! db/db* shared/deep-merge {:client-capabilities {:text-document {:hover {:content-format ["markdown"]}}}})
            (is (= {:kind  "markdown"
                    :value (join [start-code (str sym " " sig) end-code
                                  ""
                                  doc
                                  line-break
                                  (str "*[" filename "](file:///a.clj)*")])}
                   (:contents (f.hover/hover (h/file-uri "file:///a.clj") foo-row foo-col db/db*))))))
        (testing "hover arity-on-same-line? enabled"
          (testing "plain"
            (swap! db/db* shared/deep-merge {:settings {:show-docs-arity-on-same-line? false
                                                        :hover {:arity-on-same-line? true}}
                                             :client-capabilities nil})
            (is (= [{:language "clojure" :value (str sym " " sig)}
                    doc
                    filename]
                   (:contents (f.hover/hover (h/file-uri "file:///a.clj") foo-row foo-col db/db*)))))

          (testing "markdown"
            (swap! db/db* shared/deep-merge {:client-capabilities {:text-document {:hover {:content-format ["markdown"]}}}})
            (is (= {:kind  "markdown"
                    :value (join [start-code (str sym " " sig) end-code
                                  ""
                                  doc
                                  line-break
                                  (str "*[" filename "](file:///a.clj)*")])}
                   (:contents (f.hover/hover (h/file-uri "file:///a.clj") foo-row foo-col db/db*))))))

        (testing "hide-filename? enabled"
          (testing "plain"
            (swap! db/db* shared/deep-merge {:settings {:show-docs-arity-on-same-line? false
                                                        :hover {:hide-file-location? true
                                                                :arity-on-same-line? false
                                                                :clojuredocs false}}
                                             :client-capabilities nil})
            (is (= [{:language "clojure" :value sym}
                    {:language "clojure" :value sig}
                    doc]
                   (:contents (f.hover/hover (h/file-uri "file:///a.clj") foo-row foo-col db/db*)))))
          (testing "markdown"
            (swap! db/db* shared/deep-merge {:settings {:show-docs-arity-on-same-line? false
                                                        :hover {:hide-file-location? true
                                                                :arity-on-same-line? false
                                                                :clojuredocs false}}
                                             :client-capabilities {:text-document {:hover {:content-format ["markdown"]}}}})
            (is (= {:kind  "markdown"
                    :value (join [start-code sym sig end-code
                                  ""
                                  doc])}
                   (:contents (f.hover/hover (h/file-uri "file:///a.clj") foo-row foo-col db/db*))))))))
    (testing "without docs"
      (let [sym "a/bar"
            sig "[y]"
            filename (h/file-path "/a.clj")]
        (testing "show-docs-arity-on-same-line? disabled"
          (testing "plain"
            (swap! db/db* shared/deep-merge {:settings {:show-docs-arity-on-same-line? false
                                                        :hover {:hide-file-location? false
                                                                :arity-on-same-line? false
                                                                :clojuredocs false}}
                                             :client-capabilities nil})
            (is (= [{:language "clojure" :value sym}
                    {:language "clojure" :value sig}
                    filename]
                   (:contents (f.hover/hover (h/file-uri "file:///a.clj") bar-row bar-col db/db*)))))
          (testing "markdown"
            (swap! db/db* shared/deep-merge {:client-capabilities {:text-document {:hover {:content-format ["markdown"]}}}})
            (is (= {:kind  "markdown"
                    :value (join [start-code sym sig end-code
                                  line-break
                                  (str "*[" filename "](file:///a.clj)*")])}
                   (:contents (f.hover/hover (h/file-uri "file:///a.clj") bar-row bar-col db/db*))))))

        (testing "show-docs-arity-on-same-line? enabled"
          (testing "plain"
            (swap! db/db* shared/deep-merge {:settings {:show-docs-arity-on-same-line? true} :client-capabilities nil})
            (is (= [{:language "clojure" :value (str sym " " sig)}
                    filename]
                   (:contents (f.hover/hover (h/file-uri "file:///a.clj") bar-row bar-col db/db*)))))

          (testing "markdown"
            (swap! db/db* shared/deep-merge {:client-capabilities {:text-document {:hover {:content-format ["markdown"]}}}})
            (is (= {:kind "markdown"
                    :value (join [start-code (str sym " " sig) end-code
                                  line-break
                                  (str "*[" filename "](file:///a.clj)*")])}
                   (:contents (f.hover/hover (h/file-uri "file:///a.clj") bar-row bar-col db/db*))))))))
    (testing "On function usage corner cases"
      (swap! db/db* shared/deep-merge {:settings {:show-docs-arity-on-same-line? true} :client-capabilities nil})
      (let [code (h/code "(ns a)"
                         "(defn foo \"Some cool docs :foo\" [x y] x)"
                         "(defn bar \"Other cool docs :bar\" [x y] x)"
                         "(foo"
                         "  1"
                         "  |2)"
                         "(->> :foo foo |bar)"
                         "(map #(foo %1 |%2) [1 2 3])")
            [[foo-row foo-col]
             [bar-row bar-col]
             [anon-row anon-col]] (h/load-code-and-locs code)]
        (is (= [{:language "clojure"
                 :value "a/foo [x y]"}
                "Some cool docs :foo"
                "/a.clj"]
               (:contents (f.hover/hover (h/file-uri "file:///a.clj") foo-row foo-col db/db*))))
        (is (= [{:language "clojure"
                 :value "a/bar [x y]"}
                "Other cool docs :bar"
                "/a.clj"]
               (:contents (f.hover/hover (h/file-uri "file:///a.clj") bar-row bar-col db/db*))))
        (is (= [{:language "clojure"
                 :value "a/foo [x y]"}
                "Some cool docs :foo"
                "/a.clj"]
               (:contents (f.hover/hover (h/file-uri "file:///a.clj") anon-row anon-col db/db*))))))))

(ns clojure-lsp.feature.code-lens-test
  (:require
   [clojure-lsp.feature.code-lens :as f.code-lens]
   [clojure-lsp.shared :as shared]
   [clojure-lsp.test-helper.internal :as h]
   [clojure.test :refer [deftest is testing]]))

(h/reset-components-before-test)

(deftest reference-code-lens
  (testing "common lens"
    (h/load-code-and-locs (str "(ns some-ns)\n"
                               "(def foo 1)\n"
                               "(defn- foo2 []\n"
                               " foo)\n"
                               "(defn bar [a b]\n"
                               "  (+ a b (foo2)))\n"
                               "(s/defn baz []\n"
                               "  (bar 2 3))\n"))
    (swap! (h/db*) assoc :kondo-config {})
    (h/assert-submaps
      [{}
       {:range
        {:start {:line 1 :character 5} :end {:line 1 :character 8}}
        :data [(h/file-uri "file:///a.clj") 2 6]}
       {:range
        {:start {:line 2 :character 7} :end {:line 2 :character 11}}
        :data [(h/file-uri "file:///a.clj") 3 8]}
       {:range
        {:start {:line 4 :character 6} :end {:line 4 :character 9}}
        :data [(h/file-uri "file:///a.clj") 5 7]}]
      (f.code-lens/reference-code-lens (h/file-uri "file:///a.clj") (h/db))))
  (testing "defrecord"
    (h/load-code-and-locs (h/code "(defrecord MyRecord [])"
                                  "(MyRecord)"
                                  "(->MyRecord)"
                                  "(map->MyRecord)"))
    (swap! (h/db*) assoc :kondo-config {})
    (is (= [{:range
             {:start {:line 0, :character 11}, :end {:line 0, :character 19}},
             :data [(h/file-uri "file:///a.clj") 1 12]}]
           (f.code-lens/reference-code-lens (h/file-uri "file:///a.clj") (h/db)))))
  (testing "keyword definitions"
    (h/load-code-and-locs (h/code "(ns foo (:require [re-frame.core :as r]))"
                                  "(r/reg-event-db ::event identity)"
                                  "(r/reg-sub ::sub identity)"))
    (swap! (h/db*) assoc :kondo-config {})
    (h/assert-submaps
      [{}
       {:range
        {:start {:line 1, :character 16}, :end {:line 1, :character 23}},
        :data [(h/file-uri "file:///a.clj") 2 17]}
       {:range
        {:start {:line 2, :character 11}, :end {:line 2, :character 16}},
        :data [(h/file-uri "file:///a.clj") 3 12]}]
      (f.code-lens/reference-code-lens (h/file-uri "file:///a.clj") (h/db))))
  (testing "namespaces definitions"
    (h/load-code-and-locs (h/code "(ns foo) (def a)"))
    (h/load-code-and-locs (h/code "(ns bar (:require [foo :as f])) f/a"))
    (swap! (h/db*) assoc :kondo-config {})
    (h/assert-submaps
      [{:range
        {:start {:line 0, :character 4}, :end {:line 0, :character 7}}
        :data [(h/file-uri "file:///a.clj") 1 5]}]
      (f.code-lens/reference-code-lens (h/file-uri "file:///a.clj") (h/db)))))

(deftest test-code-lens-resolve
  (h/load-code-and-locs (str "(ns some-ns)\n"
                             "(def foo 1)\n"
                             "(defn- foo2 []\n"
                             " foo)\n"
                             "(defn bar [a b]\n"
                             "  (+ a b (foo2)))\n"
                             "(s/defn baz []\n"
                             "  (bar 2 3))\n"))
  (testing "references"
    (testing "empty lens"
      (is (= {:range   {:start {:line      0
                                :character 5}
                        :end   {:line      0
                                :character 12}}
              :command {:title   "0 references"
                        :command "code-lens-references"
                        :arguments [(h/file-uri "file:///a.clj") 0 5]}}
             (f.code-lens/resolve-code-lens (h/file-uri "file:///a.clj") 0 5 {:start {:line 0 :character 5} :end {:line 0 :character 12}} (h/db)))))
    (testing "some lens"
      (is (= {:range   {:start {:line      1
                                :character 5}
                        :end   {:line      1
                                :character 12}}
              :command {:title   "1 reference"
                        :command "code-lens-references"
                        :arguments [(h/file-uri "file:///a.clj") 2 6]}}
             (f.code-lens/resolve-code-lens (h/file-uri "file:///a.clj") 2 6 {:start {:line 1 :character 5} :end {:line 1 :character 12}} (h/db))))
      (is (= {:range   {:start {:line      2
                                :character 7}
                        :end   {:line      2
                                :character 11}}
              :command {:title   "1 reference"
                        :command "code-lens-references"
                        :arguments [(h/file-uri "file:///a.clj") 3 8]}}
             (f.code-lens/resolve-code-lens (h/file-uri "file:///a.clj") 3 8 {:start {:line 2 :character 7} :end {:line 2 :character 11}} (h/db)))))
    (testing "defrecord lens"
      (h/load-code-and-locs (h/code "(defrecord MyRecord [])"
                                    "(MyRecord)"
                                    "(->MyRecord)"
                                    "(map->MyRecord)"))
      (is (= {:range
              {:start {:line 1, :character 5}, :end {:line 1, :character 12}},
              :command
              {:title "3 references",
               :command "code-lens-references",
               :arguments [(h/file-uri "file:///a.clj") 1 13]}}
             (f.code-lens/resolve-code-lens
               (h/file-uri "file:///a.clj") 1 13
               {:start {:line 1 :character 5}
                :end {:line 1 :character 12}}
               (h/db))))))
  (testing "test references"
    (swap! (h/db*) shared/deep-merge {:settings {:source-paths #{(h/file-path "/project/src")
                                                                 (h/file-path "/project/test")}}})
    (testing "in src file"
      (let [src-uri (h/file-uri "file:///project/src/a.clj")
            [[start-r start-c :as start] end]
            (h/load-code-and-locs (h/code "(ns a)"
                                          "(def |aa| 1)"
                                          "aa")
                                  src-uri)
            def-range (h/->range start end)]
        (h/load-code (h/code "(ns a-test"
                             "  (:require [a]))"
                             "a/aa")
                     (h/file-uri "file:///project/test/a_test.clj"))
        (is (= {:range def-range,
                :command {:title "1 reference | 1 test",
                          :command "code-lens-references",
                          :arguments [src-uri 2 6]}}
               (f.code-lens/resolve-code-lens
                 src-uri start-r start-c
                 def-range
                 (h/db))))))
    (testing "in test file"
      (let [test-uri (h/file-uri "file:///project/test/a_test.clj")
            [[start-r start-c :as start] end]
            (h/load-code-and-locs
              (h/code "(ns a-test)"
                      "(def |test-helper| 1)"
                      "test-helper")
              test-uri)
            def-range (h/->range start end)]
        (is (= {:range def-range,
                :command {:title "1 reference",
                          :command "code-lens-references",
                          :arguments [test-uri 2 6]}}
               (f.code-lens/resolve-code-lens
                 test-uri start-r start-c
                 def-range
                 (h/db))))))))

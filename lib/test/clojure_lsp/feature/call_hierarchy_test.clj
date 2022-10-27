(ns clojure-lsp.feature.call-hierarchy-test
  (:require
   [clojure-lsp.feature.call-hierarchy :as f.call-hierarchy]
   [clojure-lsp.test-helper :as h]
   [clojure.string :as string]
   [clojure.test :refer [deftest is testing]]))

(h/reset-components-before-test)

(defn code [& strings] (string/join "\n" strings))

(def a-code
  (code "(ns some.a"
        "  (:require [some.b :as b]))"
        "(defn a-func []"
        "  (b/b-func))"))

(def b-code
  (code "(ns some.b"
        "  (:require [some.c :as c]))"
        "(defn b-func []"
        "  (c/c-func)"
        "  (c/c-func))"
        "(defn b-func-2 []"
        "  (c/c-func))"))

(def c-code
  (code "(ns some.c"
        "  (:require [some.d :as d]))"
        "(defn c-func []"
        "  (d/d-func))"))

(def d-code
  (code "(ns some.d)"
        "(defn d-func []"
        "  (println 1))"))

(def core-code
  (code "(ns clojure.core)"
        "(defn println [& args])"))

(deftest prepare
  (h/load-code-and-locs a-code (h/file-uri "file:///some/a.clj"))
  (h/load-code-and-locs b-code (h/file-uri "file:///some/b.clj"))
  (h/load-code-and-locs c-code (h/file-uri "file:///some/c.clj"))
  (h/load-code-and-locs d-code (h/file-uri "file:///some/d.clj"))
  (testing "single element"
    (let [items (f.call-hierarchy/prepare (h/file-uri "file:///some/d.clj") 2 7 (h/db))]
      (is (= 1 (count items)))
      (is (= {:name            "d-func []"
              :kind            :function
              :tags            []
              :detail          "some.d"
              :uri             (h/file-uri "file:///some/d.clj")
              :range           {:start {:line 1 :character 6} :end {:line 1 :character 12}}
              :selection-range {:start {:line 1 :character 6} :end {:line 1 :character 12}}}
             (first items))))))

(deftest incoming
  (h/load-code-and-locs a-code (h/file-uri "file:///some/a.clj"))
  (h/load-code-and-locs b-code (h/file-uri "file:///some/b.clj"))
  (h/load-code-and-locs c-code (h/file-uri "file:///some/c.clj"))
  (h/load-code-and-locs d-code (h/file-uri "file:///some/d.clj"))
  (testing "from first element"
    (h/assert-submaps
      [{:from-ranges []
        :from {:name "c-func []"
               :kind :function
               :tags []
               :detail "some.c"
               :uri (h/file-uri "file:///some/c.clj")
               :range {:start {:line 2 :character 6} :end {:line 2 :character 12}}
               :selection-range {:start {:line 3 :character 3} :end {:line 3 :character 11}}}}]
      (f.call-hierarchy/incoming (h/file-uri "file:///some/d.clj") 2 7 (h/components))))

  (testing "for multiple elements"
    (h/assert-submaps
      [{:from-ranges []
        :from {:name "b-func []"
               :kind :function
               :tags []
               :detail "some.b"
               :uri (h/file-uri "file:///some/b.clj")
               :range {:start {:line 2 :character 6} :end {:line 2 :character 12}}
               :selection-range {:start {:line 3 :character 3} :end {:line 3 :character 11}}}}
       {:from-ranges []
        :from {:name "b-func []"
               :kind :function
               :tags []
               :detail "some.b"
               :uri (h/file-uri "file:///some/b.clj")
               :range {:start {:line 2 :character 6} :end {:line 2 :character 12}}
               :selection-range {:start {:line 4 :character 3} :end {:line 4 :character 11}}}}
       {:from-ranges []
        :from {:name "b-func-2 []"
               :kind :function
               :tags []
               :detail "some.b"
               :uri (h/file-uri "file:///some/b.clj")
               :range {:start {:line 5 :character 6} :end {:line 5 :character 14}}
               :selection-range {:start {:line 6 :character 3} :end {:line 6 :character 11}}}}]
      (f.call-hierarchy/incoming (h/file-uri "file:///some/c.clj") 3 7 (h/components)))))

(deftest outgoing
  (h/load-code-and-locs core-code (h/file-uri "jar:file:///.m2/clojure.jar!/clojure/core.clj"))
  (h/load-code-and-locs a-code (h/file-uri "file:///some/a.clj"))
  (h/load-code-and-locs b-code (h/file-uri "file:///some/b.clj"))
  (h/load-code-and-locs c-code (h/file-uri "file:///some/c.clj"))
  (h/load-code-and-locs d-code (h/file-uri "file:///some/d.clj"))

  (testing "from first element"
    (h/assert-submaps
      [{:from-ranges []
        :to {:name "b-func []"
             :kind :function
             :tags []
             :detail "some.b"
             :uri (h/file-uri "file:///some/b.clj")
             :range {:start {:line 2 :character 6} :end {:line 2 :character 12}}
             :selection-range {:start {:line 3 :character 3} :end {:line 3 :character 11}}}}]
      (f.call-hierarchy/outgoing (h/file-uri "file:///some/a.clj") 3 7 (h/components))))

  (testing "for multiple elements"
    (h/assert-submaps
      [{:from-ranges []
        :to {:name "c-func []"
             :kind :function
             :tags []
             :detail "some.c"
             :uri (h/file-uri "file:///some/c.clj")
             :range {:start {:line 2 :character 6} :end {:line 2 :character 12}}
             :selection-range {:start {:line 3 :character 3} :end {:line 3 :character 11}}}}
       {:from-ranges []
        :to {:name "c-func []"
             :kind :function
             :tags []
             :detail "some.c"
             :uri (h/file-uri "file:///some/c.clj")
             :range {:start {:line 2 :character 6} :end {:line 2 :character 12}}
             :selection-range {:start {:line 4 :character 3} :end {:line 4 :character 11}}}}]
      (f.call-hierarchy/outgoing (h/file-uri "file:///some/b.clj") 3 7 (h/components))))

  (testing "with external elements"
    (h/assert-submaps
      [{:from-ranges []
        :to {:name "println [& args]"
             :kind :variable
             :tags []
             :detail "clojure.core"
             :uri (h/file-uri "zipfile:///.m2/clojure.jar::clojure/core.clj")
             :range {:start {:line 1 :character 6} :end {:line 1 :character 13}}
             :selection-range {:start {:line 2 :character 3} :end {:line 2 :character 10}}}}]
      (f.call-hierarchy/outgoing (h/file-uri "file:///some/d.clj") 2 7 (h/components)))))

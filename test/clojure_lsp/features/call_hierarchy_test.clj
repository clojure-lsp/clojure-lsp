(ns clojure-lsp.features.call-hierarchy-test
  (:require
    [clojure-lsp.db :as db]
    [clojure-lsp.feature.call-hierarchy :as f.call-hierarchy]
    [clojure-lsp.parser :as parser]
    [clojure.string :as string]
    [clojure.test :refer [deftest is testing]]))

(defn long-str [& strings] (string/join "\n" strings))

(def a-code
  (long-str "(ns some.a"
            "  (:require [some.b :as b]))"
            "(defn a-func []"
            "  (b/b-func))"))

(def b-code
  (long-str "(ns some.b"
            "  (:require [some.c :as c]))"
            "(defn b-func []"
            "  (c/c-func))"
            "(defn b-func-2 []"
            "  (c/c-func))"))

(def c-code
  (long-str "(ns some.c"
            "  (:require [some.d :as d]))"
            "(defn c-func []"
            "  (d/d-func))"))

(def d-code
  (long-str "(ns some.d)"
            "(defn d-func []"
            "  (println 1))"))

(def project-root "file:/")

(deftest prepare
  (reset! db/db {:file-envs
                 {"file://some/a.clj" (parser/find-usages a-code :clj {})
                  "file://some/b.clj" (parser/find-usages b-code :clj {})
                  "file://some/c.clj" (parser/find-usages c-code :clj {})
                  "file://some/d.clj" (parser/find-usages d-code :clj {})}
                 :documents
                 {"file://some/a.clj" {:text a-code}
                  "file://some/b.clj" {:text b-code}
                  "file://some/c.clj" {:text c-code}
                  "file://some/d.clj" {:text d-code}}})
  (testing "single element"
    (let [items (f.call-hierarchy/prepare "file://some/d.clj" 2 7 (parser/find-usages d-code :clj {}) project-root)]
      (is (= 1 (count items)))
      (is (= {:name "d-func"
              :kind :function
              :tags []
              :detail "some.d"
              :uri "file://some/d.clj"
              :range {:start {:line 1 :character 6} :end {:line 1 :character 12}}
              :selection-range {:start {:line 1 :character 6} :end {:line 1 :character 12}}}
             (first items))))))

(deftest incoming
  (reset! db/db {:documents
                 {"file://some/a.clj" {:text a-code}
                  "file://some/b.clj" {:text b-code}
                  "file://some/c.clj" {:text c-code}
                  "file://some/d.clj" {:text d-code}}
                 :file-envs
                 {"file://some/a.clj" (parser/find-usages a-code :clj {})
                  "file://some/b.clj" (parser/find-usages b-code :clj {})
                  "file://some/c.clj" (parser/find-usages c-code :clj {})
                  "file://some/d.clj" (parser/find-usages d-code :clj {})}})
  (testing "from first element"
    (let [items (f.call-hierarchy/incoming "file://some/d.clj" 2 7 project-root)]
      (is (= 1 (count items)))
      (is (= {:from-ranges []
              :from {:name "c-func"
                     :kind :function
                     :tags []
                     :detail "some.c"
                     :uri "file://some/c.clj"
                     :range {:start {:line 2 :character 6} :end {:line 2 :character 12}}
                     :selection-range {:start {:line 2 :character 6} :end {:line 2 :character 12}}}}
             (first items)))))
  (testing "for multiple element"
    (let [items (f.call-hierarchy/incoming "file://some/c.clj" 3 7 project-root)]
      (is (= 2 (count items)))
      (is (= {:from-ranges []
              :from {:name "b-func"
                     :kind :function
                     :tags []
                     :detail "some.b"
                     :uri "file://some/b.clj"
                     :range {:start {:line 2 :character 6} :end {:line 2 :character 12}}
                     :selection-range {:start {:line 2 :character 6} :end {:line 2 :character 12}}}}
             (first items)))
      (is (= {:from-ranges []
              :from {:name "b-func-2"
                     :kind :function
                     :tags []
                     :detail "some.b"
                     :uri "file://some/b.clj"
                     :range {:start {:line 4 :character 6} :end {:line 4 :character 14}}
                     :selection-range {:start {:line 4 :character 6} :end {:line 4 :character 14}}}}
             (second items))))))

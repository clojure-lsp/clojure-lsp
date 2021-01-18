(ns clojure-lsp.queries-test
  (:require
    [clojure-lsp.db :as db]
    [clojure-lsp.queries :as q]
    [clojure-lsp.test-helper :as h]
    [clojure.test :refer [deftest is testing]]
    [clojure.tools.logging :as log]))

(deftest find-element-under-cursor []
  (let [code (str "(ns a.b.c (:require [d.e.f :as |f-alias]))\n"
                  "(defn x [file|name] filename)\n"
                  "|x\n"
                  "un|known")
        [[alias-r alias-c]
         [param-r param-c]
         [x-r x-c]
         [unknown-r unknown-c]] (h/load-code-and-locs code)
        ana (:analysis @db/db)]
    (h/assert-submap
      '{:alias f-alias}
      (q/find-element-under-cursor ana "/a.clj" alias-r alias-c))
    (h/assert-submap
      '{:name x}
      (q/find-element-under-cursor ana "/a.clj" x-r x-c))
    (h/assert-submap
      '{:name filename}
      (q/find-element-under-cursor ana "/a.clj" param-r param-c))
    (h/assert-submap
      '{:name unknown}
      (q/find-element-under-cursor ana "/a.clj" unknown-r unknown-c))))

(deftest find-references-from-cursor []
  (let [code (str "(ns a.b.c (:require [d.e.f :as |f-alias]))\n"
                  "(defn |x [|filename] |filename |f-alias/foo)\n"
                  "|x |unknown unknown")
        [[alias-r alias-c]
         [x-r x-c]
         [param-r param-c]
         [param-use-r param-use-c]
         [alias-use-r alias-use-c]
         [x-use-r x-use-c]
         [unknown-r unknown-c]] (h/load-code-and-locs code)
        ana (:analysis @db/db)]
    (h/assert-submaps
      [{:name 'x :name-row x-r :name-col x-c}
       {:name 'x :name-row x-use-r :name-col x-use-c}]
      (q/find-references-from-cursor ana "/a.clj" x-r x-c true))
    (h/assert-submaps
      [{:name 'filename :name-row param-r :name-col param-c}
       {:name 'filename :name-row param-use-r :name-col param-use-c}]
      (q/find-references-from-cursor ana "/a.clj" param-r param-c true))
    (h/assert-submaps
      ['{:name unknown}]
      (q/find-references-from-cursor ana "/a.clj" unknown-r unknown-c true))
    ;; TODO kondo alias
    #_(h/assert-submaps
      [{:alias 'f-alias :name-row alias-r :name-col alias-c}
       {:alias 'f-alias :name 'foo :name-row alias-use-r :name-col alias-use-c}]
      (q/find-references-from-cursor ana "/a.clj" alias-r alias-c true))))

(deftest find-definition-from-cursor []
  (let [code (str "(ns a.b.c (:require [d.e.f :as |f-alias]))\n"
                  "(defn |x [|filename] |filename |f-alias/foo)\n"
                  "|x |unknown unknown")
        [[alias-r alias-c]
         [x-r x-c]
         [param-r param-c]
         [param-use-r param-use-c]
         [alias-use-r alias-use-c]
         [x-use-r x-use-c]
         [unknown-r unknown-c]] (h/load-code-and-locs code)
        _ (h/load-code-and-locs "(ns d.e.f) (def foo 1)" "file:///b.clj")
        ana (:analysis @db/db)]
    (h/assert-submap
      {:name 'x :name-row x-r :name-col x-c}
      (q/find-definition-from-cursor ana "/a.clj" x-use-r x-use-c))
    (h/assert-submap
      {:name 'filename :name-row param-r :name-col param-c}
      (q/find-definition-from-cursor ana "/a.clj" param-use-r param-use-c))
    (is (= nil
           (q/find-definition-from-cursor ana "/a.clj" unknown-r unknown-c)))
    (h/assert-submap
      {:name 'foo :filename "/b.clj" :ns 'd.e.f}
      (q/find-definition-from-cursor ana "/a.clj" alias-use-r alias-use-c))
    (h/assert-submap
      {:alias 'f-alias :name-row alias-r :name-col alias-c}
      (q/find-definition-from-cursor ana "/a.clj" alias-r alias-c))))


(deftest find-unused-aliases
  (testing "with single unused-alias"
    (reset! db/db {})
    (let [code (str "(ns a (:require [x :as f]))")
          _ (h/load-code-and-locs code)
          analysis (:analysis @db/db)]
      (h/assert-submaps
        [{:alias 'f,
          :from 'a,
          :to 'x}]
        (q/find-all-unused-aliases analysis))))
  (testing "with used require via alias"
    (reset! db/db {})
    (let [code (str "(ns a (:require [x :as f])) f/foo")
          _ (h/load-code-and-locs code)
          analysis (:analysis @db/db)]
      (h/assert-submaps
        []
        (q/find-all-unused-aliases analysis))))
  ;; TODO We can't known x/foo is using a full namespace and not the alias
  #_(testing "with used require via full-ns"
    (let [code (str "(ns a (:require [x :as f])) x/foo")
          _ (h/load-code-and-locs code)
          analysis (:analysis @db/db)]
      (h/assert-submaps
        [{:alias 'f,
          :from 'a,
          :to 'x}]
        (q/find-all-unused-aliases analysis)))))

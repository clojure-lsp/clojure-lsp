(ns clojure-lsp.handlers-test
  (:require
   [clojure-lsp.db :as db]
   [clojure-lsp.handlers :as handlers]
   [clojure-lsp.parser :as parser]
   [clojure.test :refer :all]
   [clojure.tools.logging :as log]
   [clojure-lsp.crawler :as crawler]))

(deftest test-rename
  (reset! db/db {:file-envs
                 {"file://a.clj" (parser/find-usages "(ns a) (def bar ::bar)" :clj {})
                  "file://b.clj" (parser/find-usages "(ns b (:require [a :as aa])) (def x aa/bar) ::aa/bar :aa/bar" :clj {})
                  "file://c.clj" (parser/find-usages "(ns c (:require [a :as aa])) (def x aa/bar)" :clj {})}})
  (testing "on symbol without namespace"
    (let [changes (:changes (handlers/rename "file://a.clj" 1 13 "foo"))]
      (is (= 1 (count (get changes "file://a.clj"))))
      (is (= 1 (count (get changes "file://b.clj"))))
      (is (= "foo" (get-in changes ["file://a.clj" 0 :new-text])))
      (is (= "aa/foo" (get-in changes ["file://b.clj" 0 :new-text])))))
  (testing "on ::keyword"
    (let [changes (:changes (handlers/rename "file://a.clj" 1 17 "foo"))]
      (is (= 1 (count (get changes "file://a.clj"))))
      (is (= 1 (count (get changes "file://b.clj"))))
      (is (= "::foo" (get-in changes ["file://a.clj" 0 :new-text])))
      (is (= "::aa/foo" (get-in changes ["file://b.clj" 0 :new-text])))))
  (testing "on symbol with namespace adds existing namespace"
    (is (= "foo" (get-in (handlers/rename "file://b.clj" 1 38 "foo")
                         [:changes "file://a.clj" 0 :new-text])))
    (is (= "aa/foo" (get-in (handlers/rename "file://b.clj" 1 38 "foo")
                            [:changes "file://b.clj" 0 :new-text]))))
  (testing "on symbol with namespace removes passed-in namespace"
    (is (= "foo" (get-in (handlers/rename "file://b.clj" 1 38 "aa/foo")
                         [:changes "file://a.clj" 0 :new-text])))
    (is (= "aa/foo" (get-in (handlers/rename "file://b.clj" 1 38 "aa/foo")
                            [:changes "file://b.clj" 0 :new-text]))))
  (testing "on alias changes namespaces inside file"
    (let [changes (:changes (handlers/rename "file://b.clj" 1 25 "xx"))]
      (is (= 0 (count (get changes "file://a.clj"))))
      (is (= 0 (count (get changes "file://c.clj"))))
      (is (= 3 (count (get changes "file://b.clj"))))
      (is (= "xx" (get-in changes ["file://b.clj" 0 :new-text])))
      (is (= "xx/bar" (get-in changes ["file://b.clj" 1 :new-text]))))))

(deftest test-find-diagnostics
  (reset! db/db {:file-envs
                 {"file://a.clj" (parser/find-usages "(ns a) (def bar ::bar)" :clj {})
                  "file://b.clj" (parser/find-usages "(ns b (:require [a :as a] [c :as c])) (def x a/bar) :a/bar" :clj {})}})
  (testing "unused symbols"
    (is (= ["Unused alias: c" "Unused declaration: b" "Unused declaration: x"]
           (map :message (crawler/find-diagnostics #{} "file://b.clj" (get-in @db/db [:file-envs "file://b.clj"])))))))

(deftest test-completion
  (let [db-state {:file-envs
                  {"file://a.cljc" (parser/find-usages
                                     (str "(ns alpaca.ns (:require [user :as alpaca]))\n"
                                          "(def barr)\n"
                                          "(def bazz)")
                                     :clj
                                     {})
                   "file://b.clj" (parser/find-usages
                                    (str "(ns user)\n"
                                         "(def alpha)\n"
                                         "alp\n"
                                         "ba")
                                    :clj
                                    {})
                   "file://c.cljs" (parser/find-usages
                                    (str "(ns alpaca.ns)\n"
                                         "(def baff)\n")
                                    :cljs
                                    {})
                   "file://d.clj" (parser/find-usages
                                    (str "(ns d (:require [alpaca.ns :as alpaca]))")
                                    :clj
                                    {})}}]
    (testing "complete-a"
      (reset! db/db db-state)
      (is (= [{:label "alpha"}
              {:label "alpaca" :detail "user"}
              {:label "alpaca" :detail "alpaca.ns"}
              {:label "alpaca.ns" :detail "alpaca.ns"}]
             (handlers/completion "file://b.clj" 3 18))))
    (testing "complete-ba"
      (reset! db/db db-state)
			(is (= [{:label "bases"}]
             (handlers/completion "file://b.clj" 4 3))))
    (testing "complete-alph"
      (reset! db/db (update-in db-state [:file-envs "file://b.clj" 4] merge {:sym 'user/alph :str "alph"}))
      (is (= [{:label "alpha"}]
             (handlers/completion "file://b.clj" 3 3))))
    (testing "complete-alpaca"
      (reset! db/db (update-in db-state [:file-envs "file://b.clj" 4] merge {:sym 'alpaca :str "alpaca"}))
      (is (= [{:label "alpaca" :detail "user"}
              {:label "alpaca" :detail "alpaca.ns"}
              {:label "alpaca.ns" :detail "alpaca.ns"}
              {:label "alpaca/barr" :detail "alpaca.ns"}
              {:label "alpaca/bazz" :detail "alpaca.ns"}]
             (handlers/completion "file://b.clj" 3 18))))
    (testing "complete-core-stuff"
      (reset! db/db (update-in db-state [:file-envs "file://b.clj" 4] merge {:sym 'freq :str "freq"}))
      (is (= [{:label "frequencies"}]
             (handlers/completion "file://b.clj" 3 18)))
      (reset! db/db (update-in db-state [:file-envs "file://b.clj" 4] merge {:sym 'Sys :str "Sys"}))
      (is (= [{:label "System"}]
             (handlers/completion "file://b.clj" 3 18))))))

(deftest test-range-formatting
    (reset! db/db {:documents {"file://a.clj" {:text "(a  )\n(b c d)"}}})
  (is (= [{:range {:start {:line 0 :character 0}
                   :end {:line 0 :character 5}}
           :new-text "(a)"}]
         (handlers/range-formatting "file://a.clj" {:row 1 :col 1 :end-row 1 :end-col 4}))))

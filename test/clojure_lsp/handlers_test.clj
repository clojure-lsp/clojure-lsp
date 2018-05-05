(ns clojure-lsp.handlers-test
  (:require
   [clojure.test :refer :all]
   [clojure-lsp.db :as db]
   [clojure-lsp.handlers :as handlers]))

(deftest test-rename
  (reset! db/db {:file-envs
                 {"file://a.clj" {:usages [{:sym 'a/bar :sexpr 'bar
                                            :row 1 :col 1 :end-row 1 :end-col 4}]}
                  "file://b.clj" {:usages [{:sym 'a/bar :sexpr 'a/bar
                                            :row 1 :col 1 :end-row 1 :end-col 6}]}}})
  (testing "rename on symbol without namespace"
    (is (= "foo" (get-in (handlers/rename "file://a.clj" 1 1 "foo")
                         [:changes "file://a.clj" 0 :new-text])))
    (is (= "a/foo" (get-in (handlers/rename "file://a.clj" 1 1 "foo")
                           [:changes "file://b.clj" 0 :new-text]))))
  (testing "rename on symbol with namespace adds existing namespace"
    (is (= "foo" (get-in (handlers/rename "file://b.clj" 1 1 "foo")
                         [:changes "file://a.clj" 0 :new-text])))
    (is (= "a/foo" (get-in (handlers/rename "file://b.clj" 1 1 "foo")
                           [:changes "file://b.clj" 0 :new-text]))))
  (testing "rename on symbol with namespace removes passed-in namespace"
    (is (= "foo" (get-in (handlers/rename "file://b.clj" 1 1 "a/foo")
                         [:changes "file://a.clj" 0 :new-text])))
    (is (= "a/foo" (get-in (handlers/rename "file://b.clj" 1 1 "a/foo")
                           [:changes "file://b.clj" 0 :new-text])))))

(deftest test-completion
  (let [db-state {:project-aliases {'alpaca.ns 'alpaca}
                  :file-envs
                  {"file://a.clj" {:ns 'alpaca.ns
                                   :usages [{:sym 'alpaca.ns/barr :sexpr 'barr :tags #{:declare :public}
                                             :row 1 :col 1 :end-row 1 :end-col 5}
                                            {:sym 'alpaca.ns/bazz :sexpr 'bazz  :tags #{:declare :public}
                                             :row 2 :col 1 :end-row 2 :end-col 5}]}
                   "file://b.clj" {:ns 'b
                                   :requires #{'alpaca.ns}
                                   :usages [{:sym 'user/alpha :sexpr 'alpha :tags #{:declare}
                                             :row 1 :col 1 :end-row 1 :end-col 2}
                                            {:sym 'user/alp :sexpr 'alp
                                             :row 2 :col 1 :end-row 2 :end-col 2}]}}}]
    (testing "complete-a"
      (reset! db/db db-state)
      (is (= [{:label "alpha"} {:label "alpaca"}]
             (handlers/completion "file://b.clj" 2 2))))
    (testing "complete-alph"
      (reset! db/db (update-in db-state [:file-envs "file://b.clj" :usages 1] merge {:sym 'user/alph :sexpr 'alph}))
      (is (= [{:label "alpha"}]
             (handlers/completion "file://b.clj" 2 2))))
    (testing "complete-alpaca"
      (reset! db/db (update-in db-state [:file-envs "file://b.clj" :usages 1] merge {:sym 'alpa :sexpr 'alpaca}))
      (is (= [{:label "alpaca/barr"} {:label "alpaca/bazz"}]
             (handlers/completion "file://b.clj" 2 2))))
    (testing "complete-core-stuff"
      (reset! db/db (update-in db-state [:file-envs "file://b.clj" :usages 1] merge {:sym 'freq :sexpr 'freq}))
      (is (= [{:label "frequencies"}]
             (handlers/completion "file://b.clj" 2 2)))
      (reset! db/db (update-in db-state [:file-envs "file://b.clj" :usages 1] merge {:sym 'Sys :sexpr 'Sys}))
      (is (= [{:label "System"}]
             (handlers/completion "file://b.clj" 2 2))))))

(deftest test-range-formatting
    (reset! db/db {:documents {"file://a.clj" {:text "(a  )\n(b c d)"}}})
  (is (= [{:range {:start {:line 0 :character 0}
                   :end {:line 0 :character 5}}
           :new-text "(a)"}]
         (handlers/range-formatting "file://a.clj" {:row 1 :col 1 :end-row 1 :end-col 4}))))

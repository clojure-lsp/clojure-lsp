(ns clojure-lsp.handlers-test
  (:require
   [clojure.test :refer :all]
   [clojure-lsp.db :as db]
   [clojure-lsp.handlers :as handlers]))

(deftest test-rename
  (reset! db/db {:file-envs
                 {"file://a.clj" {:usages [{:sym 'a/bar :str "bar"
                                            :row 1 :col 1 :end-row 1 :end-col 4}]}
                  "file://b.clj" {:usages [{:sym 'a/bar :str "a/bar"
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
                                   :usages [{:sym 'alpaca.ns/barr :str "barr" :tags #{:declare :public}
                                             :row 1 :col 1 :end-row 1 :end-col 5}
                                            {:sym 'alpaca.ns/bazz :str "bazz"  :tags #{:declare :public}
                                             :row 2 :col 1 :end-row 2 :end-col 5}]}
                   "file://b.clj" {:ns 'b
                                   :requires #{'alpaca.ns}
                                   :usages [{:sym 'user/alpha :str "alpha" :tags #{:declare}
                                             :row 1 :col 1 :end-row 1 :end-col 2}
                                            {:sym 'user/alp :str "alp"
                                             :row 2 :col 1 :end-row 2 :end-col 2}
                                            {:sym 'user/ba :str "ba"
                                             :row 3 :col 1 :end-row 3 :end-col 2}]}}}]
    (testing "complete-a"
      (reset! db/db db-state)
      (is (= [{:label "alpha"} {:label "alpaca/barr" :detail "alpaca.ns"} {:label "alpaca/bazz" :detail "alpaca.ns"}]
             (handlers/completion "file://b.clj" 2 2))))
    (testing "complete-ba"
      (reset! db/db db-state)
			(is (= [{:label "barr" :detail "alpaca.ns"
							 :text-edit {:range {:start {:line 2, :character 0}, :end {:line 2, :character 1}}, :new-text "alpaca/barr"}}
							{:label "bazz" :detail "alpaca.ns"
							 :text-edit {:range {:start {:line 2, :character 0}, :end {:line 2, :character 1}}, :new-text "alpaca/bazz"}}
              {:label "bases"}]
             (handlers/completion "file://b.clj" 3 2))))
    (testing "complete-alph"
      (reset! db/db (update-in db-state [:file-envs "file://b.clj" :usages 1] merge {:sym 'user/alph :str "alph"}))
      (is (= [{:label "alpha"}]
             (handlers/completion "file://b.clj" 2 2))))
    (testing "complete-alpaca"
      (reset! db/db (update-in db-state [:file-envs "file://b.clj" :usages 1] merge {:sym 'alpa :str "alpaca"}))
      (is (= [{:label "alpaca/barr" :detail "alpaca.ns"} {:label "alpaca/bazz" :detail "alpaca.ns"}]
             (handlers/completion "file://b.clj" 2 2))))
    (testing "complete-core-stuff"
      (reset! db/db (update-in db-state [:file-envs "file://b.clj" :usages 1] merge {:sym 'freq :str "freq"}))
      (is (= [{:label "frequencies"}]
             (handlers/completion "file://b.clj" 2 2)))
      (reset! db/db (update-in db-state [:file-envs "file://b.clj" :usages 1] merge {:sym 'Sys :str "Sys"}))
      (is (= [{:label "System"}]
             (handlers/completion "file://b.clj" 2 2))))))

(deftest test-range-formatting
    (reset! db/db {:documents {"file://a.clj" {:text "(a  )\n(b c d)"}}})
  (is (= [{:range {:start {:line 0 :character 0}
                   :end {:line 0 :character 5}}
           :new-text "(a)"}]
         (handlers/range-formatting "file://a.clj" {:row 1 :col 1 :end-row 1 :end-col 4}))))

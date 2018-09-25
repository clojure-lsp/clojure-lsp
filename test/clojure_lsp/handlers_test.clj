(ns clojure-lsp.handlers-test
  (:require
   [clojure.test :refer :all]
   [clojure-lsp.db :as db]
   [clojure-lsp.handlers :as handlers]))

(deftest test-rename
  (reset! db/db {:file-envs
                 {"file://a.clj" [{:sym 'a/bar :str "bar" :file-type :clj
                                   :row 1 :col 1 :end-row 1 :end-col 4}]
                  "file://b.clj" [{:sym 'a/bar :str "a/bar" :file-type :clj
                                   :row 1 :col 1 :end-row 1 :end-col 6}]}})
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
                  {"file://a.cljc" [{:sym 'alpaca.ns :str "alpaca.ns" :tags #{:public :ns}}
                                    {:sym 'alpaca.ns/barr :str "barr" :tags #{:declare :public} :file-type :clj
                                     :row 1 :col 1 :end-row 1 :end-col 5}
                                    {:sym 'alpaca.ns/bazz :str "bazz"  :tags #{:declare :public} :file-type :clj
                                     :row 2 :col 1 :end-row 2 :end-col 5}]
                   "file://b.clj" [{:sym 'user :str "user" :tags #{:public :ns}}
                                   {:sym 'alpaca.ns :str "alpaca.ns" :tags #{:require}}
                                   {:sym 'alpaca :str "alpaca" :tags #{:alias} :ns 'alpaca.ns}
                                   {:sym 'user/alpha :str "alpha" :tags #{:declare} :file-type :clj
                                    :row 1 :col 1 :end-row 1 :end-col 2}
                                   {:sym 'user/alp :str "alp" :file-type :clj
                                    :row 2 :col 1 :end-row 2 :end-col 2}
                                   {:sym 'user/ba :str "ba" :file-type :clj
                                    :row 3 :col 1 :end-row 3 :end-col 2}]
                   "file://c.cljs" [{:sym 'alpaca.ns/barr :str "barr" :tags #{:declare :public} :file-type :cljs
                                     :row 1 :col 1 :end-row 1 :end-col 5}
                                    {:sym 'alpaca.ns/bazz :str "bazz"  :tags #{:declare :public} :file-type :cljs
                                     :row 2 :col 1 :end-row 2 :end-col 5}]
                   "file://d.clj" [{:sym 'alpaca :str "alpaca" :tags #{:alias} :ns 'user}]}}]
    (testing "complete-a"
      (reset! db/db db-state)
      (is (= [{:label "alpha"}
              {:label "alpaca" :detail "alpaca.ns"}
              {:label "alpaca" :detail "user"}
              {:label "alpaca.ns" :detail "alpaca.ns"}]
             (handlers/completion "file://b.clj" 2 2))))
    (testing "complete-ba"
      (reset! db/db db-state)
			(is (= [{:label "bases"}]
             (handlers/completion "file://b.clj" 3 2))))
    (testing "complete-alph"
      (reset! db/db (update-in db-state [:file-envs "file://b.clj" 4] merge {:sym 'user/alph :str "alph"}))
      (is (= [{:label "alpha"}]
             (handlers/completion "file://b.clj" 2 2))))
    (testing "complete-alpaca"
      (reset! db/db (update-in db-state [:file-envs "file://b.clj" 4] merge {:sym 'alpaca :str "alpaca"}))
      (is (= [{:label "alpaca" :detail "alpaca.ns"}
              {:label "alpaca" :detail "user"}
              {:label "alpaca.ns" :detail "alpaca.ns"}
              {:label "alpaca/barr" :detail "alpaca.ns"}
              {:label "alpaca/bazz" :detail "alpaca.ns"}]
             (handlers/completion "file://b.clj" 2 2))))
    (testing "complete-core-stuff"
      (reset! db/db (update-in db-state [:file-envs "file://b.clj" 4] merge {:sym 'freq :str "freq"}))
      (is (= [{:label "frequencies"}]
             (handlers/completion "file://b.clj" 2 2)))
      (reset! db/db (update-in db-state [:file-envs "file://b.clj" 4] merge {:sym 'Sys :str "Sys"}))
      (is (= [{:label "System"}]
             (handlers/completion "file://b.clj" 2 2))))))

(deftest test-range-formatting
    (reset! db/db {:documents {"file://a.clj" {:text "(a  )\n(b c d)"}}})
  (is (= [{:range {:start {:line 0 :character 0}
                   :end {:line 0 :character 5}}
           :new-text "(a)"}]
         (handlers/range-formatting "file://a.clj" {:row 1 :col 1 :end-row 1 :end-col 4}))))

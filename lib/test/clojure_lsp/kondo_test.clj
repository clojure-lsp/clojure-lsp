(ns clojure-lsp.kondo-test
  (:require
   [clojure-lsp.kondo :as lsp.kondo]
   [clojure-lsp.shared :as shared]
   [clojure.test :refer [deftest is testing]]))

(def ^:private uri "file:///my/foo/Bar.class")

(defn ^:private java-member [name]
  {:class (String. "my.foo.Bar")
   :uri (String. uri)
   :name name
   :type (String. "int")
   :flags #{:public :static :field :final}})

(deftest normalize-analysis-java-definitions-test
  (let [analysis {:java-class-definitions [{:class (String. "my.foo.Bar")
                                            :uri (String. uri)
                                            :flags #{:public}}]
                  :java-member-definitions [(java-member "CONSTANT")
                                            (java-member "otherConstant")]}
        result (#'lsp.kondo/normalize-analysis true {} identity {} analysis)
        class-def (first (get-in result [uri :java-class-definitions]))
        [member-a member-b] (get-in result [uri :java-member-definitions])]
    (testing "elements are kept even without name positions"
      (is class-def)
      (is member-a)
      (is member-b))
    (testing "no zero positions are added"
      (is (not (contains? class-def :name-row)))
      (is (not (contains? member-a :name-row))))
    (testing "equal values share the same instance"
      (is (identical? (:class member-a) (:class member-b)))
      (is (identical? (:class class-def) (:class member-a)))
      (is (identical? (:uri member-a) (:uri member-b)))
      (is (identical? (:type member-a) (:type member-b)))
      (is (identical? (:flags member-a) (:flags member-b))))))

(deftest normalize-analysis-java-class-usages-ignore-hint-test
  ;; https://github.com/clojure-lsp/clojure-lsp/issues/2380
  ;; clj-kondo merges the expr metadata into java-class-usage elements, so a
  ;; `#_{:clj-kondo/ignore [...]}` hint leaks rewrite-clj nodes holding
  ;; functions, which transit cannot serialize when caching the db.
  (let [clj-uri "file:///a.clj"
        analysis {:java-class-usages [{:class "java.util.Date"
                                       :uri clj-uri
                                       :method-name "from"
                                       :call true
                                       :lang :clj
                                       :clj-kondo/ignore {:row 2 :col 3 :end-row 2 :end-col 41
                                                          :linters {:tag :vector :seq-fn vec}}
                                       :clj-kondo/ignore-id :G__1234
                                       :row 2 :col 42 :end-row 2 :end-col 57
                                       :name-row 2 :name-col 43 :name-end-row 2 :name-end-col 52}]}
        result (#'lsp.kondo/normalize-analysis false {} identity {} analysis)
        element (first (get-in result [clj-uri :java-class-usages]))]
    (testing "ignore hint metadata with unserializable values is dropped"
      (is element)
      (is (not (contains? element :clj-kondo/ignore)))
      (is (not (contains? element :clj-kondo/ignore-id))))))

(deftest canonicalize-java-analysis-test
  (let [clj-uri "file:///a.clj"
        clj-buckets {:var-definitions [{:name 'foo}]}
        analysis {(String. uri) {:java-class-definitions [{:class (String. "my.foo.Bar")
                                                           :uri (String. uri)
                                                           :bucket :java-class-definitions
                                                           :external? true
                                                           :flags #{:public}}]
                                 :java-member-definitions [(assoc (java-member "CONSTANT")
                                                                  :bucket :java-member-definitions
                                                                  :external? true)
                                                           (assoc (java-member "otherConstant")
                                                                  :bucket :java-member-definitions
                                                                  :external? true)]}
                  clj-uri clj-buckets}
        result (lsp.kondo/canonicalize-java-analysis analysis)
        class-def (first (get-in result [uri :java-class-definitions]))
        [member-a member-b] (get-in result [uri :java-member-definitions])]
    (testing "java definition values share the same instance"
      (is (identical? (:class member-a) (:class member-b)))
      (is (identical? (:class class-def) (:class member-a)))
      (is (identical? (:uri member-a) (:uri member-b)))
      (is (identical? (:type member-a) (:type member-b)))
      (is (identical? (:flags member-a) (:flags member-b))))
    (testing "other analysis stays untouched"
      (is (identical? clj-buckets (get result clj-uri))))))

(deftest merge-batch-results-test
  (let [batch-a {:external? true
                 :config {:linters {}}
                 :analysis {"file:///a.clj" {:var-definitions [{:name 'a}]}}
                 :findings {"file:///a.clj" [{:type :unused}]}
                 :diagnostics {:clj-kondo {"file:///a.clj" [{:type :unused}]}}}
        batch-b {:external? true
                 :config {:linters {}}
                 :analysis {"file:///b.clj" {:var-definitions [{:name 'b}]}}
                 :findings {"file:///b.clj" [{:type :unused2}]}
                 :diagnostics {:clj-kondo {"file:///b.clj" [{:type :unused2}]}}}
        merged (#'lsp.kondo/merge-batch-results batch-a batch-b)]
    (testing "shallow merge of disjoint-uri batches matches deep-merge"
      (let [deep (shared/deep-merge batch-a batch-b)]
        (is (= (:analysis deep) (:analysis merged)))
        (is (= (:findings deep) (:findings merged)))
        (is (= (get-in deep [:diagnostics :clj-kondo])
               (get-in merged [:diagnostics :clj-kondo])))))
    (testing "both batches' uris are present"
      (is (= {"file:///a.clj" {:var-definitions [{:name 'a}]}
              "file:///b.clj" {:var-definitions [{:name 'b}]}}
             (:analysis merged)))
      (is (= {"file:///a.clj" [{:type :unused}]
              "file:///b.clj" [{:type :unused2}]}
             (:findings merged))))
    (testing "scalar keys are kept from the first batch"
      (is (true? (:external? merged)))
      (is (= {:linters {}} (:config merged))))))

(deftest db-with-merged-analysis-test
  (let [java-uri "file:///dep.jar:foo/Bar.java"
        class-def {:class "foo.Bar" :uri java-uri :bucket :java-class-definitions}
        member-def {:class "foo.Bar" :name "baz" :uri java-uri :bucket :java-member-definitions}
        db {:analysis {java-uri {:java-class-definitions [class-def]}}}
        results {:external? true
                 :analysis {java-uri {:java-member-definitions [member-def]}}}]
    (testing "merging keeps buckets already present at the uri (lazy member analysis)"
      (let [merged (lsp.kondo/db-with-merged-analysis db results)]
        (is (= [class-def] (get-in merged [:analysis java-uri :java-class-definitions])))
        (is (= [member-def] (get-in merged [:analysis java-uri :java-member-definitions])))))
    (testing "db-with-analysis replaces the per-uri analysis, dropping other buckets"
      (let [replaced (lsp.kondo/db-with-analysis db results)]
        (is (nil? (get-in replaced [:analysis java-uri :java-class-definitions])))
        (is (= [member-def] (get-in replaced [:analysis java-uri :java-member-definitions])))))))

(deftest java-member-definitions-mode-test
  (testing "defaults to lazy/on-demand"
    (is (= :lazy (#'lsp.kondo/java-member-definitions-mode {})))
    (is (= :lazy (#'lsp.kondo/java-member-definitions-mode {:analysis {:java {:member-definitions :lazy}}})))
    (is (= :lazy (#'lsp.kondo/java-member-definitions-mode {:analysis {:java {:member-definitions :on-demand}}}))))
  (testing "true keeps the eager upfront analysis"
    (is (= :eager (#'lsp.kondo/java-member-definitions-mode {:analysis {:java {:member-definitions true}}}))))
  (testing "false disables member definitions entirely"
    (is (= :off (#'lsp.kondo/java-member-definitions-mode {:analysis {:java {:member-definitions false}}})))))

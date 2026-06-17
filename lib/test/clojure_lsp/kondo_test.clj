(ns clojure-lsp.kondo-test
  (:require
   [clojure-lsp.kondo :as lsp.kondo]
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
        result (#'lsp.kondo/normalize-analysis true {} {} analysis)
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

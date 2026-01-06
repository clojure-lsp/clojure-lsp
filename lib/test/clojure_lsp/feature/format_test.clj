(ns clojure-lsp.feature.format-test
  (:require
   [clojure-lsp.feature.format :as f.format]
   [clojure-lsp.shared :as shared]
   [clojure-lsp.test-helper.internal :as h]
   [clojure.test :refer [are deftest is testing]]))

(h/reset-components-before-test)

(deftest test-style-indent
  (are [index input expected]
       (= expected (#'f.format/arg-spec->cljfmt-arg index input))
    0 [:defn]       [:inner 1 0]
    0 [[:defn]]     [:inner 2 0]
    0 [[[:defn]]]   [:inner 3 0]
    2 [[[[:defn]]]] [:inner 4 2]
    0 [1]           [:inner 1 0]
    3 [[4]]         [:inner 2 3]
    1 [[[2]]]       [:inner 3 1]
    2 [[[[3]]]]     [:inner 4 2])

  (are [input expected] (= expected (#'f.format/style-indent->cljfmt-spec input))
    0                      [[:block 0]]
    1                      [[:block 1]]
    2                      [[:block 2]]
    :defn                  [[:inner 0]]
    :form                  nil

    [0]                    [[:block 0]]
    [1]                    [[:block 1]]
    [2]                    [[:block 2]]
    [:defn]                [[:inner 0]]
    [:form]                nil

       ;; letfn
    [1 [[:defn]] :form]    [[:block 1] [:inner 2 0]]
       ;; defrecord
    [2 :form :form [1]]    [[:block 2] [:inner 1]])

  (let [macro-styles {'myletfn     [1 [[:defn]] :form]
                      'mydefrecord [2 :form :form [1]]
                      'myblock1    1
                      'mydefn      :defn
                      'no-style    nil}
        analysis {"file:///project/a.clj"
                  {:var-definitions
                   (for [[sym indent] macro-styles]
                     {:name  sym
                      :ns    'some.ns
                      :macro true
                      :meta  {:style/indent indent}})}}]
    (are [sym expected] (= expected (-> {:analysis analysis}
                                        (#'f.format/extract-style-indent-metadata)
                                        :indents
                                        (get sym)))
      'myletfn             nil
      'some.ns/myletfn     [[:block 1] [:inner 2 0]]
      'some.ns/mydefrecord [[:block 2] [:inner 1]]
      'some.ns/mydefn      [[:inner 0]]
      'some.ns/myblock1    [[:block 1]]
      'some.ns/no-style    nil)))

(deftest test-formatting
  (swap! (h/db*) shared/deep-merge {:project-root-uri (h/file-uri "file:///project")})
  (h/load-code-and-locs "(a  )\n(b c d)")
  (testing "when custom config file doesn't exists"
    (with-redefs [shared/file-exists? (constantly false)]
      (is (= "(a)\n(b c d)"
             (:new-text (first (f.format/formatting (h/file-uri "file:///a.clj") (h/components))))))))
  (testing "when custom config file exists"
    (with-redefs [shared/file-exists? (constantly true)
                  slurp (constantly "{}")]
      (is (= "(a)\n(b c d)"
             (:new-text (first (f.format/formatting (h/file-uri "file:///a.clj") (h/components)))))))))

(deftest test-formatting-noop
  (swap! (h/db*) shared/deep-merge {:project-root-uri (h/file-uri "file:///project")})
  (h/load-code-and-locs "(a)\n(b c d)")
  (with-redefs [shared/file-exists? (constantly false)]
    (let [r (f.format/formatting (h/file-uri "file:///a.clj") (h/components))]
      (is (empty? r))
      (is (vector? r)))))

(defn range-formatting [code]
  (let [[[row col] [end-row end-col] :as positions] (h/load-code-and-locs code)]
    (let [position-count (count positions)]
      (assert (= 2 position-count) (format "Expected two cursors, got %s" position-count)))
    (f.format/range-formatting (h/file-uri "file:///a.clj") {:row row :col col :end-row end-row :end-col end-col} (h/db))))

(deftest test-range-formatting
  (swap! (h/db*) shared/deep-merge {:project-root-uri (h/file-uri "file:///project")})
  (testing "when custom config file doesn't exists"
    (with-redefs [shared/file-exists? (constantly false)]
      (is (= [{:range {:start {:line 0 :character 0}
                       :end {:line 0 :character 5}}
               :new-text "(a)"}]
             (range-formatting "|(a | )\n(b c d)")))))
  (testing "when custom config file exists"
    (with-redefs [shared/file-exists? (constantly true)
                  slurp (constantly "{}")]
      (is (= [{:range {:start {:line 0 :character 0}
                       :end {:line 0 :character 5}}
               :new-text "(a)"}]
             (range-formatting "|(a | )\n(b c d)")))))
  (testing "of several forms"
    (is (= [{:range {:start {:line 0 :character 0}
                     :end {:line 1 :character 5}}
             :new-text "(a)\n(b)"}]
           (range-formatting "(a | )\n|(b  )\n(c d)")))
    (is (= [{:range {:start {:line 0 :character 0}
                     :end {:line 1 :character 5}}
             :new-text "(a)\n(b)"}]
           (range-formatting "(a | )\n(|b  )\n(c d)")))
    (is (= [{:range {:start {:line 0 :character 0}
                     :end {:line 1 :character 0}}
             :new-text "(a)\n"}]
           (range-formatting "(a | )|\n(b  )\n(c d)")))
    (is (= [{:range {:start {:line 0 :character 0}
                     :end {:line 0 :character 5}}
             :new-text "(a)"}]
           (range-formatting "(a | |)\n(b  )\n(c d)")))
    (is (= [{:range {:start {:line 0 :character 0}
                     :end {:line 1 :character 0}}
             :new-text "(a)\n"}]
           (range-formatting "|(a  )|\n(b  )\n(c d)")))
    (is (= [{:range {:start {:line 2 :character 0}
                     :end {:line 2 :character 5}}
             :new-text "(c d)"}]
           (range-formatting "(a  )\n(b  )\n|(c d)|"))))
  (testing "when parens are unbalenced"
    (is (= nil
           (let [[[row col] [end-row end-col]] (h/load-code-and-locs "(str :foo :bar :baz))")]
             (f.format/range-formatting (h/file-uri "file:///a.clj") {:row row :col col :end-row end-row :end-col end-col} (h/db)))))))

(ns clojure-lsp.feature.inlay-hints-test
  (:require
   [clojure-lsp.feature.inlay-hints :as f.inlay-hints]
   [clojure-lsp.shared :as shared]
   [clojure-lsp.test-helper.internal :as h]
   [clojure.test :refer [deftest is testing]]))

(h/reset-components-before-test)

(def full-range
  {:start {:line 0 :character 0}
   :end {:line 100 :character 0}})

(defn hint [row col label]
  {:position (shared/row-col->position row col)
   :label label
   :kind :parameter})

(deftest parameter-name-hints
  (let [[[first-row first-col]
         [second-row second-col]] (h/load-code-and-locs
                                    (h/code "(defn greet [name punctuation]"
                                            "  (str name punctuation))"
                                            "(greet |\"Ada\" |\"!\")"))]
    (is (= [(hint first-row first-col "name:")
            (hint second-row second-col "punctuation:")]
           (f.inlay-hints/hints (h/file-uri "file:///a.clj") full-range (h/components))))))

(deftest arity-selection
  (testing "selects an exact fixed arity"
    (let [[[first-row first-col]
           [second-row second-col]] (h/load-code-and-locs
                                      (h/code "(defn choose"
                                              "  ([one] one)"
                                              "  ([one two] two))"
                                              "(choose |1 |2)"))]
      (is (= [(hint first-row first-col "one:")
              (hint second-row second-col "two:")]
             (f.inlay-hints/hints (h/file-uri "file:///a.clj") full-range (h/components))))))

  (testing "reuses the rest parameter name"
    (let [[[first-row first-col]
           [second-row second-col]
           [third-row third-col]] (h/load-code-and-locs
                                    (h/code "(defn collect [head & tail] tail)"
                                            "(collect |1 |2 |3)"))]
      (is (= [(hint first-row first-col "head:")
              (hint second-row second-col "tail:")
              (hint third-row third-col "tail:")]
             (f.inlay-hints/hints (h/file-uri "file:///a.clj") full-range (h/components))))))

  (testing "omits calls without one unambiguous arity"
    (h/load-code-and-locs
      (h/code "(defn ambiguous"
              "  ([value] value)"
              "  ([value & more] more))"
              "(ambiguous |1)"))
    (is (= []
           (f.inlay-hints/hints (h/file-uri "file:///a.clj") full-range (h/components)))))

  (testing "omits calls without a compatible arity"
    (h/load-code-and-locs
      (h/code "(defn fixed [one two] two)"
              "(fixed |1 |2 |3)"))
    (is (= []
           (f.inlay-hints/hints (h/file-uri "file:///a.clj") full-range (h/components))))))

(deftest conservative-hints
  (testing "omits destructured and intentionally unused parameters"
    (let [[[_map-row _map-col]
           [value-row value-col]
           [_ignored-row _ignored-col]] (h/load-code-and-locs
                                          (h/code "(defn consume [{:keys [value]} result _ignored]"
                                                  "  result)"
                                                  "(consume |{} |1 |2)"))]
      (is (= [(hint value-row value-col "result:")]
             (f.inlay-hints/hints (h/file-uri "file:///a.clj") full-range (h/components))))))

  (testing "omits macros"
    (h/load-code-and-locs
      (h/code "(defmacro with-value [value] value)"
              "(with-value |1)"))
    (is (= []
           (f.inlay-hints/hints (h/file-uri "file:///a.clj") full-range (h/components))))))

(deftest requested-range
  (let [[[_first-row _first-col]
         [second-row second-col]] (h/load-code-and-locs
                                    (h/code "(defn greet [name punctuation]"
                                            "  (str name punctuation))"
                                            "(greet |\"Ada\" |\"!\")"))
        second-position (shared/row-col->position second-row second-col)
        range {:start second-position
               :end (update second-position :character inc)}]
    (is (= [(hint second-row second-col "punctuation:")]
           (f.inlay-hints/hints (h/file-uri "file:///a.clj") range (h/components))))))

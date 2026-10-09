(ns clojure-lsp.feature.inlay-hints-test
  (:require
   [clojure-lsp.feature.inlay-hints :as f.inlay-hints]
   [clojure-lsp.shared :as shared]
   [clojure-lsp.test-helper.internal :as h]
   [clojure.string :as string]
   [clojure.test :refer [deftest is testing]]))

(h/reset-components-before-test)

(def full-range
  {:start {:line 0 :character 0}
   :end {:line 100 :character 0}})

(defn hint [row col label]
  {:position (shared/row-col->position row col)
   :label label
   :kind :parameter})

(defn type-hint [row col label]
  {:position (shared/row-col->position row col)
   :label label
   :kind :type
   :padding-left true})

(defn install-java-members! [members]
  (swap! (h/db*) assoc-in [:analysis "file:///java-members.jar" :java-member-definitions] members))

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
    (let [[[arity-row arity-col]
           [first-row first-col]
           [second-row second-col]] (h/load-code-and-locs
                                      (h/code "(defn choose"
                                              "  ([one] one)"
                                              "  ([one two] two))"
                                              "(choose| |1 |2)"))]
      (is (= [(hint first-row first-col "one:")
              (hint second-row second-col "two:")
              (type-hint arity-row arity-col ": [one two]")]
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
  (testing "shows destructuring and omits intentionally unused parameters"
    (let [[[map-row map-col]
           [value-row value-col]
           [_ignored-row _ignored-col]] (h/load-code-and-locs
                                          (h/code "(defn consume [{:keys [value]} result _ignored]"
                                                  "  result)"
                                                  "(consume |{} |1 |2)"))]
      (is (= [(hint map-row map-col "{:keys [value]}:")
              (hint value-row value-col "result:")]
             (f.inlay-hints/hints (h/file-uri "file:///a.clj") full-range (h/components))))))

  (testing "omits macros"
    (h/load-code-and-locs
      (h/code "(defmacro with-value [value] value)"
              "(with-value |1)"))
    (is (= []
           (f.inlay-hints/hints (h/file-uri "file:///a.clj") full-range (h/components)))))

  (testing "omits hints when argument and parameter names match"
    (let [[[_name-row _name-col]
           [punctuation-row punctuation-col]] (h/load-code-and-locs
                                                (h/code "(defn greet [name punctuation]"
                                                        "  (str name punctuation))"
                                                        "(let [name \"Ada\"]"
                                                        "  (greet |name |\"!\"))"))]
      (is (= [(hint punctuation-row punctuation-col "punctuation:")]
             (f.inlay-hints/hints (h/file-uri "file:///a.clj") full-range (h/components)))))))

(deftest destructuring-and-keywords
  (testing "vector destructuring"
    (let [[[row col]] (h/load-code-and-locs
                        (h/code "(defn pair [[left right]] [left right])"
                                "(pair |[1 2])"))]
      (is (= [(hint row col "[left right]:")]
             (f.inlay-hints/hints (h/file-uri "file:///a.clj") full-range (h/components))))))

  (testing "keyword arguments from a trailing map"
    (let [[[row col]] (h/load-code-and-locs
                        (h/code "(defn opts [& {:keys [active verbose]}] active)"
                                "(opts |:active true :verbose false)"))]
      (is (= [(hint row col "{:keys [active verbose]}:")]
             (f.inlay-hints/hints (h/file-uri "file:///a.clj") full-range (h/components)))))))

(deftest namespace-hints
  (let [[[row col]] (h/load-code-and-locs
                      (h/code "(ns sample"
                              "  (:require [clojure.string :as string :refer [join]]))"
                              "(join| \",\" [\"a\"])"
                              "(string/join \",\" [\"b\"])"
                              "(clojure.string/join \",\" [\"c\"])"
                              "(str \"c\")"))
        hints (f.inlay-hints/hints (h/file-uri "file:///a.clj") full-range (h/components))]
    (is (= [(type-hint row col ": clojure.string")]
           (filterv #(= ": clojure.string" (:label %)) hints)))
    (is (not-any? #(string/includes? (:label %) "clojure.core") hints))))

(defn java-member [class name parameter-types parameters return-type flags]
  {:class class
   :name name
   :parameter-types parameter-types
   :parameters parameters
   :return-type return-type
   :flags flags})

(deftest java-hints
  (testing "selects the overload and shows parameter and return type"
    (let [[[arg-row arg-col]
           [end-row end-col]] (h/load-code-and-locs
                                (h/code "(Integer/parseInt |\"10\")|"))]
      (install-java-members!
        [(java-member "java.lang.Integer" "parseInt"
                      ["java.lang.String"] ["String s"] "int" #{:public :static :method})
         (java-member "java.lang.Integer" "parseInt"
                      ["java.lang.String" "int"] ["String s" "int radix"] "int" #{:public :static :method})])
      (is (= [(hint arg-row arg-col "s:")
              (type-hint end-row end-col ": int")]
             (f.inlay-hints/hints (h/file-uri "file:///a.clj") full-range (h/components))))))

  (testing "omits an ambiguous overload"
    (h/load-code-and-locs (h/code "(Math/abs |1)"))
    (install-java-members!
      [(java-member "java.lang.Math" "abs" ["int"] ["int a"] "int" #{:public :static :method})
       (java-member "java.lang.Math" "abs" ["long"] ["long a"] "long" #{:public :static :method})])
    (is (= []
           (f.inlay-hints/hints (h/file-uri "file:///a.clj") full-range (h/components)))))

  (testing "static field type"
    (let [[[row col]] (h/load-code-and-locs
                        (h/code "Integer/MAX_VALUE|"))]
      (install-java-members!
        [(java-member "java.lang.Integer" "MAX_VALUE"
                      nil nil "int" #{:public :static :final :field})])
      (is (= [(type-hint row col ": int")]
             (f.inlay-hints/hints (h/file-uri "file:///a.clj") full-range (h/components))))))

  (testing "constructor parameter"
    (let [[[arg-row arg-col]] (h/load-code-and-locs
                                (h/code "(String. |\"hello\")"))]
      (install-java-members!
        [(java-member "java.lang.String" "<init>"
                      ["java.lang.String"] ["String value"] "void" #{:public :method})])
      (is (= [(hint arg-row arg-col "value:")]
             (f.inlay-hints/hints (h/file-uri "file:///a.clj") full-range (h/components))))))

  (testing "instance method on a string literal"
    (let [[[begin-row begin-col]
           [end-row end-col]
           [return-row return-col]] (h/load-code-and-locs
                                      (h/code "(.substring \"hello\" |1 |3)|"))]
      (install-java-members!
        [(java-member "java.lang.String" "substring"
                      ["int"] ["int beginIndex"] "java.lang.String" #{:public :method})
         (java-member "java.lang.String" "substring"
                      ["int" "int"] ["int beginIndex" "int endIndex"] "java.lang.String" #{:public :method})])
      (is (= [(hint begin-row begin-col "beginIndex:")
              (hint end-row end-col "endIndex:")
              (type-hint return-row return-col ": String")]
             (f.inlay-hints/hints (h/file-uri "file:///a.clj") full-range (h/components))))))

  (testing "class inferred from a local type hint"
    (let [[[receiver-row receiver-col]
           [return-row return-col]] (h/load-code-and-locs
                                      (h/code "(let [^String s \"hello\"]"
                                              "  (.length s|)|)"))]
      (install-java-members!
        [(java-member "java.lang.String" "length"
                      [] [] "int" #{:public :method})])
      (is (= [(type-hint receiver-row receiver-col ": String")
              (type-hint return-row return-col ": int")]
             (f.inlay-hints/hints (h/file-uri "file:///a.clj") full-range (h/components)))))))

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

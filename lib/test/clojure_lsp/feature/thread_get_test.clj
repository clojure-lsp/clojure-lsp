(ns clojure-lsp.feature.thread-get-test
  (:require
   [clojure-lsp.feature.thread-get :as f.thread-get]
   [clojure-lsp.test-helper.internal :as h]
   [clojure.test :refer [deftest is]]))

(h/reset-components-before-test)

(defn get-in-more-code [code]
  (f.thread-get/get-in-more (h/load-code-and-zloc code)))

(defn get-in-all-code [code]
  (f.thread-get/get-in-all (h/load-code-and-zloc code)))

(defn get-in-less-code [code]
  (f.thread-get/get-in-less (h/load-code-and-zloc code)))

(defn get-in-none-code [code]
  (f.thread-get/get-in-none (h/load-code-and-zloc code)))

(defn ^:private can-get-in-more-code? [code]
  (f.thread-get/can-get-in-more? (h/load-code-and-zloc code)))

(defn ^:private can-get-in-less-code? [code]
  (f.thread-get/can-get-in-less? (h/load-code-and-zloc code)))

(defn- as-string [changes]
  (h/changes->code changes (h/db)))

(defmacro ^:private assert-get-in-more [expected code]
  `(let [expected# ~expected
         code# ~code]
     (is (can-get-in-more-code? code#) code#)
     (is (= expected#
            (as-string (get-in-more-code code#)))
         code#)))

(defmacro ^:private assert-get-in-all [expected code]
  `(let [expected# ~expected
         code# ~code]
     (is (can-get-in-more-code? code#) code#)
     (is (= expected#
            (as-string (get-in-all-code code#)))
         code#)))

(defmacro ^:private assert-get-in-less [expected code]
  `(let [expected# ~expected
         code# ~code]
     (is (can-get-in-less-code? code#) code#)
     (is (= expected#
            (as-string (get-in-less-code code#)))
         code#)))

(defmacro ^:private assert-get-in-none [expected code]
  `(let [expected# ~expected
         code# ~code]
     (is (can-get-in-less-code? code#) code#)
     (is (= expected#
            (as-string (get-in-none-code code#)))
         code#)))

(deftest can-get-in-more-test
  (is (not (can-get-in-more-code? "|:nope")))
  (is (not (can-get-in-more-code? "|(get m :x)")))
  (is (not (can-get-in-more-code? "|(get-in m [:x :y :z])")))
  (is (not (can-get-in-more-code? "|(a 1 2 3)"))))

(deftest get-in-more-test
  (assert-get-in-more "(get (:y (:x m)) :z)" "|(:z (:y (:x m)))")
  (assert-get-in-more "(get-in (:x m) [:y :z])" "|(get (:y (:x m)) :z)")
  (assert-get-in-more "(get-in m [:x :y :z])" "|(get-in (:x m) [:y :z])"))

(deftest get-in-all-test
  (assert-get-in-all "(get m :x)" "|(:x m)")
  (assert-get-in-all "(get-in m [:x :y :z])" "|(:z (:y (:x m)))"))

(deftest can-get-in-less-test
  (is (not (can-get-in-less-code? "|:nope")))
  (is (not (can-get-in-less-code? "|(:z (:y (:x m)))")))
  (is (not (can-get-in-less-code? "|(get-in m [])"))))

(deftest get-in-less-test
  (assert-get-in-less "(get-in (:x m) [:y :z])" "|(get-in m [:x :y :z])")
  (assert-get-in-less "(get (:y (:x m)) :z)" "|(get-in (:x m) [:y :z])")
  (assert-get-in-less "(:z (:y (:x m)))" "|(get (:y (:x m)) :z)"))

(deftest get-in-none-test
  (assert-get-in-none "(:x m)" "|(get m :x)")
  (assert-get-in-none "(:z (:y (:x m)))" "|(get-in m [:x :y :z])"))

(deftest default-values-test
  (is (not (can-get-in-less-code? "|(get m x 1)")))
  (assert-get-in-less "(:x m 1)" "|(get m :x 1)")
  (assert-get-in-less "('x m 1)" "|(get m 'x 1)")
  (assert-get-in-less "(:x m 1)" "|(get-in m [:x] 1)")
  (assert-get-in-less "(get (:x m) :y 1)" "|(get-in m [:x :y] 1)")
  (assert-get-in-less "(get-in (:x m) [:y :z] 1)" "|(get-in m [:x :y :z] 1)")
  (assert-get-in-none "(:y (:x m) 1)" "|(get-in m [:x :y] 1)")
  (assert-get-in-none "(:z (:y (:x m)) 1)" "|(get-in m [:x :y :z] 1)")

  (assert-get-in-more "(get m :x 1)" "|(:x m 1)")
  (assert-get-in-more "(get-in m [:y :z] 1)" "|(get (:y m) :z 1)")
  (assert-get-in-more "(get-in m [:x :y :z] 1)" "|(get-in (:x m) [:y :z] 1)")
  (assert-get-in-all "(get-in (:x m 1) [:y :z])" "|(:z (:y (:x m 1)))")
  (assert-get-in-all "(get (:y (:x m) 1) :z)" "|(:z (:y (:x m) 1))")
  (assert-get-in-all "(get-in m [:x :y :z] 1)" "|(:z (:y (:x m)) 1)")
  (is (not (can-get-in-more-code? "|(get (:y m 2) :z 1)")))
  (is (not (can-get-in-more-code? "|(get-in (:x m 2) [:y :z] 1)"))))

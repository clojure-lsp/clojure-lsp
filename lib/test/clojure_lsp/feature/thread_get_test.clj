(ns clojure-lsp.feature.thread-get-test
  (:require
   [clojure-lsp.feature.thread-get :as f.thread-get]
   [clojure-lsp.test-helper :as h]
   [clojure.test :refer [deftest is]]))

(defn ^:private do-get-in-more [code]
  (let [components (h/make-components)
        zloc (h/load-code-and-zloc code h/default-uri components)
        db (h/db components)]
    {:can (-> zloc f.thread-get/can-get-in-more?)
     :did (-> zloc f.thread-get/get-in-more (h/changes->code db))}))

(defn ^:private do-get-in-all [code]
  (let [components (h/make-components)
        zloc (h/load-code-and-zloc code h/default-uri components)
        db (h/db components)]
    {:can (-> zloc f.thread-get/can-get-in-more?)
     :did (-> zloc f.thread-get/get-in-all (h/changes->code db))}))

(defn ^:private do-get-in-less [code]
  (let [components (h/make-components)
        zloc (h/load-code-and-zloc code h/default-uri components)
        db (h/db components)]
    {:can (-> zloc f.thread-get/can-get-in-less?)
     :did (-> zloc f.thread-get/get-in-less (h/changes->code db))}))

(defn ^:private do-get-in-none [code]
  (let [components (h/make-components)
        zloc (h/load-code-and-zloc code h/default-uri components)
        db (h/db components)]
    {:can (-> zloc f.thread-get/can-get-in-less?)
     :did (-> zloc f.thread-get/get-in-none (h/changes->code db))}))

(defmacro ^:private assert-cannot [doer code]
  `(let [result# (~doer ~code)]
     (is (not (:can result#)) (:did result#))))

(defmacro ^:private assert-did [doer restructured-code original-code]
  `(let [original# ~original-code
         result# (~doer original#)]
     (is (:can result#) original#)
     (is (= ~restructured-code (:did result#)) original#)))

(deftest can-get-in-more-test
  (assert-cannot do-get-in-more "|:nope")
  (assert-cannot do-get-in-more "|(get m :x)")
  (assert-cannot do-get-in-more "|(get-in m [:x :y :z])")
  (assert-cannot do-get-in-more "|(a 1 2 3)"))

(deftest get-in-more-test
  (assert-did do-get-in-more "(get (:y (:x m)) :z)" "|(:z (:y (:x m)))")
  (assert-did do-get-in-more "(get-in (:x m) [:y :z])" "|(get (:y (:x m)) :z)")
  (assert-did do-get-in-more "(get-in m [:x :y :z])" "|(get-in (:x m) [:y :z])"))

(deftest get-in-all-test
  (assert-did do-get-in-all "(get m :x)" "|(:x m)")
  (assert-did do-get-in-all "(get-in m [:x :y :z])" "|(:z (:y (:x m)))"))

(deftest can-get-in-less-test
  (assert-cannot do-get-in-less "|:nope")
  (assert-cannot do-get-in-less "|(:z (:y (:x m)))")
  (assert-cannot do-get-in-less "|(get-in m [])"))

(deftest get-in-less-test
  (assert-did do-get-in-less "(get-in (:x m) [:y :z])" "|(get-in m [:x :y :z])")
  (assert-did do-get-in-less "(get (:y (:x m)) :z)" "|(get-in (:x m) [:y :z])")
  (assert-did do-get-in-less "(:z (:y (:x m)))" "|(get (:y (:x m)) :z)"))

(deftest get-in-none-test
  (assert-did do-get-in-none "(:x m)" "|(get m :x)")
  (assert-did do-get-in-none "(:z (:y (:x m)))" "|(get-in m [:x :y :z])"))

(deftest default-values-test
  (assert-cannot do-get-in-less "|(get m x 1)")
  (assert-did do-get-in-less "(:x m 1)" "|(get m :x 1)")
  (assert-did do-get-in-less "('x m 1)" "|(get m 'x 1)")
  (assert-did do-get-in-less "(:x m 1)" "|(get-in m [:x] 1)")
  (assert-did do-get-in-less "(get (:x m) :y 1)" "|(get-in m [:x :y] 1)")
  (assert-did do-get-in-less "(get-in (:x m) [:y :z] 1)" "|(get-in m [:x :y :z] 1)")
  (assert-did do-get-in-none "(:y (:x m) 1)" "|(get-in m [:x :y] 1)")
  (assert-did do-get-in-none "(:z (:y (:x m)) 1)" "|(get-in m [:x :y :z] 1)")

  (assert-did do-get-in-more "(get m :x 1)" "|(:x m 1)")
  (assert-did do-get-in-more "(get-in m [:y :z] 1)" "|(get (:y m) :z 1)")
  (assert-did do-get-in-more "(get-in m [:x :y :z] 1)" "|(get-in (:x m) [:y :z] 1)")
  (assert-did do-get-in-all "(get-in (:x m 1) [:y :z])" "|(:z (:y (:x m 1)))")
  (assert-did do-get-in-all "(get (:y (:x m) 1) :z)" "|(:z (:y (:x m) 1))")
  (assert-did do-get-in-all "(get-in m [:x :y :z] 1)" "|(:z (:y (:x m)) 1)")
  (assert-cannot do-get-in-more "|(get (:y m 2) :z 1)")
  (assert-cannot do-get-in-more "|(get-in (:x m 2) [:y :z] 1)"))

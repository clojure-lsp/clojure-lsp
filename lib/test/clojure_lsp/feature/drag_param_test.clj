(ns clojure-lsp.feature.drag-param-test
  (:require
   [clojure-lsp.feature.drag-param :as f.drag-param]
   [clojure-lsp.test-helper.internal :as h]
   [clojure.test :refer [deftest is testing]]))

(h/reset-components-before-test)

(defn can-drag-zloc-backward? [zloc]
  (f.drag-param/can-drag-backward? zloc h/default-uri (h/db)))

(defn can-drag-zloc-forward? [zloc]
  (f.drag-param/can-drag-forward? zloc h/default-uri (h/db)))

(defn can-drag-code-backward? [code]
  (can-drag-zloc-backward? (h/load-code-and-zloc code)))

(defn can-drag-code-forward? [code]
  (can-drag-zloc-forward? (h/load-code-and-zloc code)))

(defn drag-zloc-backward [{:keys [zloc position]}]
  (f.drag-param/drag-backward zloc position h/default-uri (h/components)))

(defn drag-zloc-forward [{:keys [zloc position]}]
  (f.drag-param/drag-forward zloc position h/default-uri (h/components)))

(defn drag-code-backward [code]
  (drag-zloc-backward (h/load-code-into-zloc-and-position code)))

(defn drag-code-forward [code]
  (drag-zloc-forward (h/load-code-into-zloc-and-position code)))

(defn- as-string [change]
  (some-> change
          :changes-by-uri
          (get h/default-uri)
          (h/changes->code (h/db))))

(defn- as-position [change]
  (when-let [{:keys [row col end-row end-col]} (some-> change
                                                       :show-document-after-edit
                                                       :range)]
    [[row col] [end-row end-col]]))

;; These are macros so test failures have the right line numbers
(defmacro assert-drag-backward [expected code]
  `(let [moved#         (drag-code-backward ~code)
         [text# [pos#]] (h/positions-from-text ~expected)]
     (is (= text# (as-string moved#)))
     ;; Range should only be single char
     (is (= [pos# pos#] (as-position moved#)))))
(defmacro assert-drag-forward [expected code]
  `(let [moved#         (drag-code-forward ~code)
         [text# [pos#]] (h/positions-from-text ~expected)]
     (is (= text# (as-string moved#)))
     (is (= [pos# pos#] (as-position moved#)))))

(defmacro assert-cannot-drag-code [code]
  `(let [code# ~code]
     (is (not (can-drag-code-backward? code#)) (as-string (drag-code-backward code#)))
     (is (not (can-drag-code-forward? code#)) (as-string (drag-code-forward code#)))))

;; These are only the negative cases, proving when drag-param-forward/backward is
;; NOT offered in the actions menu. The positive cases are all tested indirectly
;; via assert-drag-forward, since if a movement happens, it implicitly passed
;; can-drag-param-forward/backward?
(deftest should-not-always-offer-to-drag
  (testing "from non-params vector"
    (assert-cannot-drag-code "[1 |2 3]")
    (assert-cannot-drag-code "(let [a 1 |b 2 c 3])"))
  (testing "from maps, sets, lists"
    (assert-cannot-drag-code "{:a 1 |:b 2 :c 3}")
    (assert-cannot-drag-code "#{1 |2 3}")
    (assert-cannot-drag-code "(1 |2 3)")
    (assert-cannot-drag-code "(cond a 1 |b 2 c 3)"))
  (testing "on first/last entry"
    (is (not (can-drag-code-backward? "(defn f [|a b])")))
    (is (not (can-drag-code-forward? "(defn f [a |b])"))))
  (testing "to/from vararg"
    (assert-cannot-drag-code "(defn f [& |a])")
    (assert-cannot-drag-code "(defn f [|& a])")
    (is (not (can-drag-code-backward? "(defn f [a & |b])")))
    (is (not (can-drag-code-forward? "(defn f [|a & b])"))))
  (testing "from multi-arity"
    (assert-cannot-drag-code "(defn f ([a |b c]))")))

(deftest should-drag
  (testing "without usages"
    (assert-drag-backward (h/code "(defn f [|b a c])")
                          (h/code "(defn f [a |b c])"))
    (assert-drag-forward (h/code "(defn f [a c |b])")
                         (h/code "(defn f [a |b c])")))
  (testing "with usages"
    (assert-drag-backward (h/code "(defn f [|b a c])"
                                  "(f 2 1 3)")
                          (h/code "(defn f [a |b c])"
                                  "(f 1 2 3)"))
    (assert-drag-forward (h/code "(defn f [a c |b])"
                                 "(f 1 3 2)")
                         (h/code "(defn f [a |b c])"
                                 "(f 1 2 3)")))
  (testing "with hard-to-process usages"
    ;; within partial, but not involving partialed location
    (assert-drag-backward (h/code "(defn f [|b a c])"
                                  "(partial f 2 1)")
                          (h/code "(defn f [a |b c])"
                                  "(partial f 1 2)"))
    (assert-drag-forward (h/code "(defn f [b |a c])"
                                 "(partial f 2 1)")
                         (h/code "(defn f [|a b c])"
                                 "(partial f 1 2)"))
    ;; from partialed location
    (is (nil? (drag-code-backward (h/code "(defn f [a b |c])"
                                          "(partial f 1 2)"))))
    ;; to partialed location
    (is (nil? (drag-code-forward (h/code "(defn f [a |b c])"
                                         "(partial f 1 2)"))))
    ;; from threaded spot
    (is (nil? (drag-code-forward (h/code "(defn f [|a b c])"
                                         "(-> a (f :b :c))"))))
    (is (nil? (drag-code-backward (h/code "(defn f [a b |c])"
                                          "(->> c (f :a :b))"))))
    ;; to threaded spot
    (is (nil? (drag-code-forward (h/code "(defn f [a |b c])"
                                         "(->> c (f :a :b))"))))
    (is (nil? (drag-code-backward (h/code "(defn f [a |b c])"
                                          "(-> a (f :b :c))"))))
    ;; from applied location
    (is (nil? (drag-code-backward (h/code "(defn f [a |b c])"
                                          "(apply f args)"))))
    (is (nil? (drag-code-backward (h/code "(defn f [a |b c])"
                                          "(apply f :a args)"))))
    ;; to applied location
    (is (nil? (drag-code-forward (h/code "(defn f [a |b c])"
                                         "(apply f args)"))))
    (is (nil? (drag-code-forward (h/code "(defn f [a |b c])"
                                         "(apply f :a args)"))))
    ;; in function lookup
    (is (nil? (drag-code-forward (h/code "(defn f [a |b c])"
                                         "(defn exec [k a b c]"
                                         "  (let [fns {:x f :y g :z h}]"
                                         "    ((get fns k) a b c)))")))))
  (testing "from defmacro"
    (assert-drag-backward (h/code "(defmacro f [|b a c])")
                          (h/code "(defmacro f [a |b c])"))
    (assert-drag-forward (h/code "(defmacro f [a c |b])")
                         (h/code "(defmacro f [a |b c])")))
  (testing "from defn-"
    (assert-drag-backward (h/code "(defn- f [|b a c])")
                          (h/code "(defn- f [a |b c])"))
    (assert-drag-forward (h/code "(defn- f [a c |b])")
                         (h/code "(defn- f [a |b c])")))
  (testing "from defn with metadata"
    (assert-drag-backward (h/code "(defn ^:private f [|b a c])")
                          (h/code "(defn ^:private f [a |b c])"))
    (assert-drag-forward (h/code "(defn ^:private f [a c |b])")
                         (h/code "(defn ^:private f [a |b c])")))
  (testing "with varargs"
    (assert-drag-backward (h/code "(defn f [|b a & c])")
                          (h/code "(defn f [a |b & c])"))
    (assert-drag-forward (h/code "(defn f [b |a & c])")
                         (h/code "(defn f [|a b & c])"))))

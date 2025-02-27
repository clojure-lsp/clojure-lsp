(ns clojure-lsp.feature.paredit-test
  (:require
   [clojure-lsp.feature.paredit :as f.paredit]
   [clojure-lsp.test-helper :as h]
   [clojure.test :refer [are deftest is]]))

(defmacro ^:private assert-op [op expected code]
  `(let [applied# (~op h/default-uri (:zloc (h/load-code-into-zloc-and-position ~code)))
         [text# _#] (h/positions-from-text ~expected)]
     (is (= text#
            (h/changes->code (-> applied# :changes-by-uri first second) (h/db))))))

(defn ^:private edit
  [paredit-fn code]
  (let [{{row :row col :col} :position zloc :zloc} (h/load-code-into-zloc-and-position code)
        transformations (paredit-fn h/default-uri zloc row col)
        {{actual-row :row actual-col :col} :range} (:show-document-after-edit transformations)
        result (h/changes->code (-> transformations :changes-by-uri first second) (h/db))]
    (if transformations
      (h/put-cursor-at result actual-row actual-col)
      "|")))

(deftest forward-slurp-test
  (are [expected code] (= expected (edit f.paredit/forward-slurp code))
    "[1 [2 3| 4]]" "[1 [2 3|] 4]"
    "[1 [2 [|3 4 5]] 6]" "[1 [2 [|3 4] 5] 6]"
    "[|1 2]" "[|1] 2"
    "|1" "|1"
    "|" "|"
    "{:a {|:b 1} 2}" "{:a {|:b 1}} 2"
    "{:a {:b {|:c :d}} 2}" "{:a {:b {|:c :d}}} 2"
    ;; These two below are going to fail because of a bug in rewrite-clj
    ;; "(get {}| :a)" "(get {}|) :a"
    ;; "(get {} |:a)" "(get {} |) :a"
    "(get []| 0)" "(get []|) 0"
    "(get [] |0)" "(get [] |) 0"))

(deftest forward-barf-test
  (assert-op f.paredit/forward-barf
             (h/code "[1 [2] 3| 4]")
             (h/code "[1 [2 3|] 4]"))
  (assert-op f.paredit/forward-barf
             (h/code "[1 [2 [|3 4] 5] 6]")
             (h/code "[1 [2 [|3 4 5]] 6]"))
  (assert-op f.paredit/forward-barf
             (h/code "[|1] 2")
             (h/code "[|1 2]"))
  (assert-op f.paredit/forward-barf
             (h/code "|1")
             (h/code "|1"))
  (assert-op f.paredit/forward-barf
             (h/code "|")
             (h/code "|")))

(deftest backward-slurp-test
  (are [expected code] (= expected (edit f.paredit/backward-slurp code))
    "[[1 |2 3] 4]" "[1 [|2 3] 4]"
    "[1 [[2 |3 4] 5] 6]" "[1 [2 [|3 4] 5] 6]"
    "[|1] 2" "[|1] 2"
    "|1" "|1"
    "|" "|"))

(deftest backward-barf-test
  (assert-op f.paredit/backward-barf
             (h/code "[1 |2 [3] 4]")
             (h/code "[1 [|2 3] 4]"))
  (assert-op f.paredit/backward-barf
             (h/code "[1 [2 [|3 4] 5] 6]")
             (h/code "[1 [[2 |3 4] 5] 6]"))
  (assert-op f.paredit/backward-barf
             (h/code "1 [] 2")
             (h/code "[|1] 2"))
  (assert-op f.paredit/backward-barf
             (h/code "|1")
             (h/code "|1"))
  (assert-op f.paredit/backward-barf
             (h/code "|")
             (h/code "|")))

(deftest raise-test
  (assert-op f.paredit/raise
             (h/code "[1 [2 |3 5] 6]")
             (h/code "[1 [2 [|3 4] 5] 6]"))
  (assert-op f.paredit/raise
             (h/code "|1")
             (h/code "|1"))
  (assert-op f.paredit/raise
             (h/code "|")
             (h/code "|")))

(deftest kill-test
  (assert-op f.paredit/kill
             (h/code "[1 [2 [|] 5] 6]")
             (h/code "[1 [2 [|3 4] 5] 6]"))
  (assert-op f.paredit/kill
             (h/code "|1")
             (h/code "|1"))
  (assert-op f.paredit/kill
             (h/code "|")
             (h/code "|")))

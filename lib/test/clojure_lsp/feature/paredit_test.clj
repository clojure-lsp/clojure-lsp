(ns clojure-lsp.feature.paredit-test
  (:require
   [clojure-lsp.feature.paredit :as f.paredit]
   [clojure-lsp.test-helper :as h]
   [clojure.test :refer [deftest is]]))

(defmacro ^:private assert-op [op expected code]
  `(let [applied# (~op (:zloc (h/load-code-into-zloc-and-position ~code)))
         [text# _#] (h/positions-from-text ~expected)]
     (is (= text# (h/changes->code applied# (h/db))))))

(deftest forward-slurp-test
  (assert-op f.paredit/forward-slurp
             (h/code "[1 [2 3| 4]]")
             (h/code "[1 [2 3|] 4]"))
  (assert-op f.paredit/forward-slurp
             (h/code "[1 [2 [|3 4 5]] 6]")
             (h/code "[1 [2 [|3 4] 5] 6]"))
  (assert-op f.paredit/forward-slurp
             (h/code "[|1 2]")
             (h/code "[|1] 2"))
  (assert-op f.paredit/forward-slurp
             (h/code "|1")
             (h/code "|1"))
  (assert-op f.paredit/forward-slurp
             (h/code "|")
             (h/code "|")))

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
  (assert-op f.paredit/backward-slurp
             (h/code "[[1 |2 3] 4]")
             (h/code "[1 [|2 3] 4]"))
  (assert-op f.paredit/backward-slurp
             (h/code "[1 [[2 |3 4] 5] 6]")
             (h/code "[1 [2 [|3 4] 5] 6]"))
  (assert-op f.paredit/backward-slurp
             (h/code "[|1] 2")
             (h/code "[|1] 2"))
  (assert-op f.paredit/backward-slurp
             (h/code "|1")
             (h/code "|1"))
  (assert-op f.paredit/backward-slurp
             (h/code "|")
             (h/code "|")))

[[1 2 3] 4]

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
             (h/code "|")
             (h/code "|1"))
  (assert-op f.paredit/kill
             (h/code "|")
             (h/code "|")))

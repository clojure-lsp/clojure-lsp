(ns clojure-lsp.feature.paredit-test
  (:require
   [clojure-lsp.feature.paredit :as f.paredit]
   [clojure-lsp.test-helper :as h]
   [clojure.test :refer [are deftest is]]))

(defmacro ^:private assert-op [op expected code]
  `(let [{{row# :row col# :col} :position zloc# :zloc} (h/load-code-into-zloc-and-position ~code)
         applied# (~op h/default-uri zloc# row# col#)
         [text# _#] (h/positions-from-text ~expected)]
     (is (= text#
            (h/changes->code (-> applied# :changes-by-uri first second) (h/db))))))

(defn ^:private pareditfy
  [paredit-fn code]
  (let [{{row :row col :col} :position zloc :zloc} (h/load-code-into-zloc-and-position code)]
    (if-let [transformations (paredit-fn h/default-uri zloc row col)]
      (let [{{actual-row :row actual-col :col} :range} (:show-document-after-edit transformations)
            result (h/changes->code (-> transformations :changes-by-uri first second) (h/db))]
        (h/put-cursor-at result actual-row actual-col))
      code)))

(deftest forward-slurp-test
  (are [expected code] (= expected (pareditfy f.paredit/forward-slurp code))
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
  (are [expected code] (= expected (pareditfy f.paredit/forward-barf code))
    "[1 [2] 3| 4]" "[1 [2 3|] 4]"
    "[12 [34] 5|6 78]" "[12 [34 5|6] 78]"
    "[1 [2 [|3 4] 5] 6]" "[1 [2 [|3 4 5]] 6]"
    "[|1] 2" "[|1 2]"
    "|1" "|1"
    "|" "|"
    "(get []) |:a" "(get [] |:a)"
    "(get []) |:a" "(get []| :a)"
    #_()))

(deftest backward-slurp-test
  (are [expected code] (= expected (pareditfy f.paredit/backward-slurp code))
    "[[1 |2 3] 4]" "[1 [|2 3] 4]"
    "[1 [[2 |3 4] 5] 6]" "[1 [2 [|3 4] 5] 6]"
    "[|1] 2" "[|1] 2"
    "|1" "|1"
    "|" "|"
    #_()))

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
  ;; FIXME: the scenario below fails
  ;; (assert-op f.paredit/raise
  ;;            (h/code "[1 [2 3| 5] 6]")
  ;;            (h/code "[1 [2 [3| 4] 5] 6]"))
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

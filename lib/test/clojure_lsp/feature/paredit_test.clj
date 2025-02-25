(ns clojure-lsp.feature.paredit-test
  (:require
   [clojure-lsp.feature.paredit :as f.paredit]
   [clojure-lsp.test-helper :as h]
   [clojure.test :refer [deftest is]]))

(defmacro ^:private assert-op [op expected code]
  `(let [applied# (~op h/default-uri (:zloc (h/load-code-into-zloc-and-position ~code)))
         [text# _#] (h/positions-from-text ~expected)]
     (is (= text#
            (h/changes->code (-> applied# :changes-by-uri first second) (h/db))))))

(comment
  (require '[rewrite-clj.paredit :as paredit]
           '[rewrite-clj.zip :as z])

  (-> "(get {}|) :a"
      (h/load-code-into-zloc-and-position)
      :zloc
      (paredit/slurp-forward-into {:from :current})
      z/root-string)

  (-> "(get {}| :a)"
      #_"(get {}|) :a"
      h/positions-from-text)

  (h/changes->code (->> "(get {}|) :a"
                        h/load-code-into-zloc-and-position
                        :zloc
                        (f.paredit/forward-slurp h/default-uri)
                        :changes-by-uri
                        first
                        second)
                   (h/db))


  )

(deftest forward-slurp-test
  #_#_#_#_#_#_#_(assert-op f.paredit/forward-slurp
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
             (h/code "|"))
  (assert-op f.paredit/forward-slurp
             (h/code "{:a {|:b 1} 2}")
             (h/code "{:a {|:b 1}} 2"))
  (assert-op f.paredit/forward-slurp
             (h/code "{:a {:b {|:c :d}} 2}")
             (h/code "{:a {:b {|:c :d}}} 2"))
  (assert-op f.paredit/forward-slurp
             (h/code "(get {}| :a)")
             (h/code "(get {}|) :a"))
  #_(assert-op f.paredit/forward-slurp
             (h/code "(get {} |) :a")
             (h/code "(get {} |:a)"))
  #_(assert-op f.paredit/forward-slurp
             (h/code "(get []|) 0")
             (h/code "(get []| 0)"))
  #_(assert-op f.paredit/forward-slurp
             (h/code "(get [] |) 0")
             (h/code "(get [] |0)")))

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
             (h/code "|1")
             (h/code "|1"))
  (assert-op f.paredit/kill
             (h/code "|")
             (h/code "|")))

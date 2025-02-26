(ns clojure-lsp.feature.paredit-test
  (:require
   [clojure-lsp.feature.paredit :as f.paredit]
   [clojure-lsp.test-helper :as h]
   [clojure.string :as string]
   [clojure.test :refer [deftest is]]
   [rewrite-clj.zip :as z]))

(defmacro ^:private assert-op [op expected code]
  `(let [applied# (~op h/default-uri (:zloc (h/load-code-into-zloc-and-position ~code)))
         [text# _#] (h/positions-from-text ~expected)]
     (is (= text#
            (h/changes->code (-> applied# :changes-by-uri first second) (h/db))))))

(defn ^:private position-cursor
  [s row col]
  (let [lines (string/split s #"\n" -1)
        line (nth lines (dec row))]
    (string/join "\n"
                 (concat
                   (subvec lines 0 (dec row))
                   [(str (subs line 0 (dec col))
                         "|"
                         (subs line (dec col)))]
                   (subvec lines row)))))

(defmacro ^:private assert-op-pos
  [op expected code]
  `(let [{{row# :row col# :col} :position zloc# :zloc} (h/load-code-into-zloc-and-position ~code)
         applied# (~op h/default-uri
                       zloc#
                       row#
                       col#)
         {{actual-row# :row actual-col# :col} :range} (:show-document-after-edit applied#)
         actual# (h/changes->code (-> applied#
                                      :changes-by-uri
                                      first
                                      second)
                                  (h/db))
         actual-string# (if applied#
                          (position-cursor actual# actual-row# actual-col#)
                          "|")]
     (is (= ~expected
            actual-string#))))

(comment
  (require '[rewrite-clj.paredit :as paredit]
           '[rewrite-clj.zip :as z])

  (-> "(get {}) :a"
      z/of-string*
      z/node
      meta)

  ;;   12
  (-> ":a"
      z/of-string
      z/root
      meta)

  ;;   12345
  (-> ":a :b"
      z/of-string
      z/root
      meta)

  ;;   123456789 0123 4
  (-> "(get {}) \"foo\""
      z/of-string
      z/root
      meta)
  ;;   12345678901
  (-> "(get {}) :a"
      z/of-string
      z/root
      meta)
  ;;   12345678901234
  (-> "(get {}) ::b/c"
      z/of-string
      z/root
      meta)
  ;;   1234567890
  (-> "(get []) 0"
      z/of-string
      z/root
      meta)
  ;;   12345678901
  (-> "(get []) 'a"
      z/of-string
      z/root
      meta)
  ;;   12345678901
  (-> "(get []) {}"
      z/of-string
      z/root
      meta)
  ;;   12345678901
  (-> "(get []) ()"
      z/of-string
      z/root
      meta)
  ;;   1234567890123
  (-> "(get []) #_{}"
      z/of-string
      z/root
      meta)

  (count "(get []) 0")
  (count "(get {}) :a")

  (-> "(get {}|) :a"
      (h/load-code-into-zloc-and-position)
      ;; :zloc
      ;; (paredit/slurp-forward-into {:from :current})
      ;; z/root-string
      #_())

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

  (macroexpand-1 '(assert-op2 f.paredit/forward-slurp
                              (h/code "(get {}| :a)")
                              (h/code "(get {}|) :a")))
  #_())

(deftest slurp-forward-test
  (assert-op-pos f.paredit/forward-slurp
                 (h/code "|")
                 (h/code "|"))
  #_(let [code (h/code "(get []|) 0")
          {{:keys [row col]} :position zloc :zloc} (h/load-code-into-zloc-and-position code)
          applied (f.paredit/forward-slurp h/default-uri
                                           zloc
                                           row
                                           col)
          _ (println 'applied= applied)
          {{actual-row :row actual-col :col} :range} (:show-document-after-edit applied)
          actual (h/changes->code (-> applied
                                      :changes-by-uri
                                      first
                                      second)
                                  (h/db))
          expected "(get []| 0)"]
      (is (= (h/positions-from-text expected)
             [actual [[actual-row actual-col]]]))))

(deftest forward-slurp-test
  (assert-op-pos f.paredit/forward-slurp
                 (h/code "[1 [2 3| 4]]")
                 (h/code "[1 [2 3|] 4]"))
  (assert-op-pos f.paredit/forward-slurp
                 (h/code "[1 [2 [|3 4 5]] 6]")
                 (h/code "[1 [2 [|3 4] 5] 6]"))
  (assert-op-pos f.paredit/forward-slurp
                 (h/code "[|1 2]")
                 (h/code "[|1] 2"))
  (assert-op-pos f.paredit/forward-slurp
                 (h/code "|1")
                 (h/code "|1"))
  (assert-op-pos f.paredit/forward-slurp
                 (h/code "|")
                 (h/code "|"))
  (assert-op-pos f.paredit/forward-slurp
                 (h/code "{:a {|:b 1} 2}")
                 (h/code "{:a {|:b 1}} 2"))
  (assert-op-pos f.paredit/forward-slurp
                 (h/code "{:a {:b {|:c :d}} 2}")
                 (h/code "{:a {:b {|:c :d}}} 2"))
  ;; These two below are going to fail because of a bug in rewrite-clj
  ;; (assert-op2 f.paredit/forward-slurp
  ;;             (h/code "(get {}| :a)")
  ;;             (h/code "(get {}|) :a"))
  ;; (assert-op2 f.paredit/forward-slurp
  ;;               (h/code "(get {} |:a)")
  ;;               (h/code "(get {} |) :a"))
  (assert-op-pos f.paredit/forward-slurp
                 (h/code "(get []| 0)")
                 (h/code "(get []|) 0"))
  (assert-op-pos f.paredit/forward-slurp
                 (h/code "(get [] |0)")
                 (h/code "(get [] |) 0")))

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

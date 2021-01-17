(ns clojure-lsp.queries-test
  (:require
    [clojure-lsp.db :as db]
    [clojure-lsp.queries :as q]
    [clojure-lsp.test-helper :as h]
    [clojure.test :refer [deftest]]
    [clojure.tools.logging :as log]))

(deftest find-element-under-cursor []
  (let [code (str "(ns a.b.c (:require [d.e.f :as |f-alias]))\n"
                  "(defn x [file|name] filename)\n"
                  "un|known")
        [[alias-r alias-c]
         [param-r param-c]
         [unknown-r unknown-c]] (h/load-code-and-locs code)
        ana (:analysis @db/db)]
    (h/assert-submap
      '{:name filename}
      (q/find-element-under-cursor ana "/a.clj" alias-r alias-c))
    (h/assert-submap
      '{:name filename}
      (q/find-element-under-cursor ana "/a.clj" param-r param-c))
    (h/assert-submap
      '{:name unknown}
      (q/find-element-under-cursor ana "/a.clj" unknown-r unknown-c))))

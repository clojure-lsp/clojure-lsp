(ns clojure-lsp.dep-graph-test
  (:require
   [clojure-lsp.dep-graph :as dep-graph]
   [clojure-lsp.feature.file-management :as f.file-management]
   [clojure-lsp.shared :as shared]
   [clojure-lsp.test-helper :as h]
   [clojure.test :refer [are deftest is testing]]))

(h/reset-components-before-test)

(defn load-code [file & lines]
  (h/load-code-and-locs
    (apply h/code lines)
    (h/file-uri (str "file://" file))))

(deftest maintaining-dep-graph
  (testing "initial external analysis"
    (h/reset-components!)
    (h/load-code-and-locs (h/code "(ns xxx"
                                  "  (:require [xxx.yyy]))")
                          (h/file-uri "jar:file:///some.jar!/xxx.clj"))
    (h/load-code-and-locs "(ns xxx.yyy)"
                          (h/file-uri "jar:file:///some.jar!/xxx/yyy.clj"))
    (is (= '{xxx {:dependencies {clojure.core 1, xxx.yyy 1}
                  :internal? false
                  :uris #{"zipfile:///some.jar::xxx.clj"}}
             xxx.yyy {:aliases {nil 1}
                      :dependencies {clojure.core 1}
                      :dependents {xxx 1}
                      :dependents-internal? false
                      :dependents-langs {:clj 1}
                      :internal? false
                      :uris #{"zipfile:///some.jar::xxx/yyy.clj"}}
             clojure.core {:aliases {nil 2},
                           :dependents {xxx 1, xxx.yyy 1},
                           :dependents-internal? false,
                           :dependents-langs {:clj 2}}}
           (:dep-graph (h/db))))
    (h/assert-submap
      '{:internal? false, :langs #{:clj}, :namespaces #{xxx} :filename "/some.jar:xxx.clj"}
      (get-in (h/db) [:documents "zipfile:///some.jar::xxx.clj"]))
    (h/assert-submap
      '{:internal? false, :langs #{:clj}, :namespaces #{xxx.yyy} :filename "/some.jar:xxx/yyy.clj"}
      (get-in (h/db) [:documents "zipfile:///some.jar::xxx/yyy.clj"])))
  (testing "initial internal analysis"
    (h/reset-components!)
    (load-code "/aaa.clj"
               "(ns aaa"
               " (:require [bbb :as b]"
               "           [ccc :as c]))")
    (load-code "/bbb.clj"
               "(ns bbb"
               " (:require [ccc :as c]))")
    (load-code "/ccc.clj"
               "(ns ccc)")
    (is (= '{aaa {:dependencies {clojure.core 1, bbb 1, ccc 1}
                  :internal? true
                  :uris #{"file:///aaa.clj"}}
             bbb {:aliases {b 1}
                  :dependencies {clojure.core 1, ccc 1}
                  :dependents {aaa 1}
                  :dependents-internal? true
                  :dependents-langs {:clj 1}
                  :internal? true
                  :uris #{"file:///bbb.clj"}}
             ccc {:aliases {c 2}
                  :dependencies {clojure.core 1}
                  :dependents {aaa 1, bbb 1}
                  :dependents-internal? true
                  :dependents-langs {:clj 2}
                  :internal? true
                  :uris #{"file:///ccc.clj"}}
             clojure.core {:aliases {nil 3},
                           :dependents {aaa 1, bbb 1, ccc 1},
                           :dependents-internal? true,
                           :dependents-langs {:clj 3}}}
           (:dep-graph (h/db))))
    (h/assert-submap
      '{:internal? true, :langs #{:clj}, :namespaces #{aaa}, :filename "/aaa.clj"}
      (get-in (h/db) [:documents "file:///aaa.clj"]))
    (h/assert-submap
      '{:internal? true, :langs #{:clj}, :namespaces #{bbb}, :filename "/bbb.clj"}
      (get-in (h/db) [:documents "file:///bbb.clj"]))
    (h/assert-submap
      '{:internal? true, :langs #{:clj}, :namespaces #{ccc}, :filename "/ccc.clj"}
      (get-in (h/db) [:documents "file:///ccc.clj"])))
  (testing "extending initial external analysis with internal analysis"
    (h/reset-components!)
    (h/load-code-and-locs (h/code "(ns xxx"
                                  "  (:require [xxx.yyy]))")
                          (h/file-uri "jar:file:///some.jar!/xxx.clj"))
    (h/load-code-and-locs "(ns xxx.yyy)"
                          (h/file-uri "jar:file:///some.jar!/xxx/yyy.clj"))
    (let [db (h/db)
          xxx (get-in db [:dep-graph 'xxx])
          xxx-yyy (get-in db [:dep-graph 'xxx.yyy])]
      (is (not (:internal? xxx)))
      (is (not (:dependents-internal? xxx)))
      (is (not (:internal? xxx-yyy)))
      (is (not (:dependents-internal? xxx-yyy))))
    (load-code "/aaa.clj"
               "(ns aaa"
               " (:require [xxx :as x]))")
    (let [db (h/db)
          xxx (get-in db [:dep-graph 'xxx])
          xxx-yyy (get-in db [:dep-graph 'xxx.yyy])]
      (is (not (:internal? xxx)))
      (is (:dependents-internal? xxx)) ;; <-- this is the change
      (is (not (:internal? xxx-yyy)))
      (is (not (:dependents-internal? xxx-yyy)))))
  (testing "adding dependency"
    (h/reset-components!)
    (load-code "/bbb.clj"
               "(ns bbb)")
    (load-code "/aaa.clj"
               "(ns aaa)")
    (let [db (h/db)]
      (is (= '{clojure.core 1} (get-in db [:dep-graph 'aaa :dependencies])))
      (is (empty? (get-in db [:dep-graph 'bbb :dependents]))))
    (load-code "/aaa.clj"
               "(ns aaa"
               " (:require [bbb :as b]))")
    (let [db (h/db)]
      (is (get-in db [:dep-graph 'aaa :dependencies 'bbb]))
      (is (get-in db [:dep-graph 'bbb :dependents 'aaa]))))
  (testing "removing dependency"
    (h/reset-components!)
    (load-code "/bbb.clj"
               "(ns bbb)")
    (load-code "/aaa.clj"
               "(ns aaa"
               " (:require [bbb :as b]))")
    (let [db (h/db)]
      (is (get-in db [:dep-graph 'aaa :dependencies 'bbb]))
      (is (get-in db [:dep-graph 'bbb :dependents 'aaa])))
    (load-code "/aaa.clj"
               "(ns aaa)")
    (let [db (h/db)]
      (is (= '{clojure.core 1} (get-in db [:dep-graph 'aaa :dependencies])))
      (is (empty? (get-in db [:dep-graph 'bbb :dependents])))))
  (testing "removing duplicate dependency"
    (h/reset-components!)
    (load-code "/bbb.clj"
               "(ns bbb)")
    (load-code "/aaa.clj"
               "(ns aaa"
               " (:require [bbb :as b]"
               "           [bbb :as b2]))")
    (let [db (h/db)]
      (is (get-in db [:dep-graph 'aaa :dependencies 'bbb]))
      (is (get-in db [:dep-graph 'bbb :dependents 'aaa]))
      (is (= '{b 1, b2 1} (get-in db [:dep-graph 'bbb :aliases]))))
    (load-code "/aaa.clj"
               "(ns aaa"
               " (:require [bbb :as b]))")
    (let [db (h/db)]
      (is (get-in db [:dep-graph 'aaa :dependencies 'bbb]))
      (is (get-in db [:dep-graph 'bbb :dependents 'aaa]))
      (is (= '{b 1} (get-in db [:dep-graph 'bbb :aliases])))))
  (testing "deleting file"
    (h/reset-components!)
    (load-code "/bbb.clj"
               "(ns bbb)")
    (load-code "/aaa.clj"
               "(ns aaa"
               " (:require [bbb :as b]))")
    (load-code "/ccc.clj"
               "(ns ccc"
               " (:require [aaa :as a]))")
    (let [db (h/db)]
      (is (seq (get-in db [:dep-graph 'aaa :dependencies])))
      (is (= '{ccc 1} (get-in db [:dep-graph 'aaa :dependents])))
      (is (seq (get-in db [:dep-graph 'aaa :uris])))
      (is (seq (get-in db [:dep-graph 'bbb :dependents])))
      (is (not (nil? (get-in db [:documents "file:///aaa.clj"])))))
    #_(alter-var-root #'db/diagnostics-chan (constantly (async/chan 1)))
    (with-redefs [shared/file-exists? (constantly false)]
      (f.file-management/did-close "file:///aaa.clj" (h/components)))
    (let [db (h/db)]
      (is (empty? (get-in db [:dep-graph 'aaa :dependencies])))
      (is (= '{ccc 1} (get-in db [:dep-graph 'aaa :dependents]))) ;; <-- no change, because ccc stil depends on aaa, even though aaa is now undefined
      (is (empty? (get-in db [:dep-graph 'aaa :uris])))
      (is (empty? (get-in db [:dep-graph 'bbb :dependents])))
      (is (nil? (get-in db [:documents "file:///aaa.clj"])))))
  (testing "in implicit user ns"
    (h/reset-components!)
    (load-code "/scratch.clj"
               "(def x 1)")
    (let [db (h/db)]
      (is (= #{"file:///scratch.clj"} (get-in db [:dep-graph 'user :uris])))
      (is (= '#{user} (get-in db [:documents "file:///scratch.clj" :namespaces])))))
  (testing "with implicit dependency on clojure.core"
    (h/reset-components!)
    (load-code "/aaa.clj"
               "(ns aaa)")
    (let [db (h/db)]
      (is (get-in db [:dep-graph 'aaa :dependencies 'clojure.core]))
      (is (get-in db [:dep-graph 'clojure.core :dependents 'aaa]))))
  (testing "with implicit dependency on cljs.core"
    (h/reset-components!)
    (load-code "/aaa.cljs"
               "(ns aaa)")
    (let [db (h/db)]
      (is (get-in db [:dep-graph 'aaa :dependencies 'cljs.core]))
      (is (get-in db [:dep-graph 'cljs.core :dependents 'aaa])))))

(deftest uri-filtering
  (h/reset-components!)
  (testing "internal namespaces"
    (load-code "/aaa.clj"
               "(ns aaa"
               " (:require [bbb :as b]"
               "           [ccc :as c]))")
    (load-code "/bbb.clj"
               "(ns bbb"
               " (:require [ccc :as c]))")
    (load-code "/ccc.clj"
               "(ns ccc)")
    (let [db (h/db)]
      (are [expected namespace]
           (= expected (dep-graph/ns-uris db namespace))
        #{"file:///aaa.clj"} 'aaa
        #{"file:///bbb.clj"} 'bbb
        #{"file:///ccc.clj"} 'ccc)
      (are [expected namespace]
           (= expected (dep-graph/ns-dependencies-uris db namespace))
        #{"file:///bbb.clj" "file:///ccc.clj"} 'aaa
        #{"file:///ccc.clj"}                   'bbb
        #{}                                    'ccc)
      (are [expected namespace]
           (= expected (dep-graph/ns-dependents-uris db namespace))
        #{}                                    'aaa
        #{"file:///aaa.clj"}                   'bbb
        #{"file:///aaa.clj" "file:///bbb.clj"} 'ccc)
      (are [expected namespace]
           (= expected (dep-graph/ns-and-dependents-uris db namespace))
        #{"file:///aaa.clj"}                                     'aaa
        #{"file:///aaa.clj" "file:///bbb.clj"}                   'bbb
        #{"file:///aaa.clj" "file:///bbb.clj" "file:///ccc.clj"} 'ccc)
      (is (= #{"file:///aaa.clj" "file:///bbb.clj"}
             (dep-graph/nses-uris db '#{aaa bbb})))
      (is (= #{"file:///aaa.clj" "file:///bbb.clj" "file:///ccc.clj"}
             (dep-graph/nses-and-dependents-uris db '#{bbb ccc})))))
  (testing "external namespaces"
    (h/reset-components!)
    (load-code "/aaa.clj" "(ns aaa)")
    (h/load-code-and-locs "(ns bbb)" (h/file-uri "jar:file:///some.jar!/bbb.clj"))
    (let [db (h/db)]
      (is (= ["file:///aaa.clj"] (dep-graph/internal-uris db)))))
  (testing "namespaces defined internally and externally"
    (h/reset-components!)
    (load-code "/aaa.clj" "(ns aaa)")
    (h/load-code-and-locs "(ns aaa)" (h/file-uri "jar:file:///some.jar!/aaa.clj"))
    (let [db (h/db)]
      (is (= #{"file:///aaa.clj" "zipfile:///some.jar::aaa.clj"} (dep-graph/ns-uris db 'aaa)))
      (is (= ["file:///aaa.clj"] (dep-graph/ns-internal-uris db 'aaa)))
      (is (= ["file:///aaa.clj"] (dep-graph/internal-uris db)))))
  (testing "file with multiple namespaces"
    (h/reset-components!)
    (load-code "/aaa.clj"
               "(ns aaa)"
               "(ns aaa2)")
    (let [db (h/db)]
      (is (= #{"file:///aaa.clj"} (dep-graph/ns-uris db 'aaa)))
      (is (= #{"file:///aaa.clj"} (dep-graph/ns-uris db 'aaa2)))
      (is (= '#{aaa aaa2} (get-in db [:documents "file:///aaa.clj" :namespaces])))))
  (testing "namespace with multiple files"
    (h/reset-components!)
    (load-code "/aaa.clj" "(ns aaa)")
    (load-code "/also/aaa.clj" "(ns aaa)")
    (let [db (h/db)]
      (is (= #{"file:///aaa.clj" "file:///also/aaa.clj"} (dep-graph/ns-uris db 'aaa)))
      (is (= '#{aaa} (get-in db [:documents "file:///aaa.clj" :namespaces])))
      (is (= '#{aaa} (get-in db [:documents "file:///also/aaa.clj" :namespaces]))))))

(deftest test-ns-aliases
  (h/load-code-and-locs "(ns aaa (:require [bbb :as b] [ccc :as c]))" (h/file-uri "file:///aaa.clj"))
  (h/load-code-and-locs "(ns bbb (:require [ccc :as c]))" (h/file-uri "file:///bbb.clj"))
  (h/load-code-and-locs "(ns ccc)" (h/file-uri "file:///ccc.clj"))
  (is (= '#{{:alias c :to ccc}
            {:alias b :to bbb}}
         (dep-graph/ns-aliases (h/db)))))

(deftest ns-aliases-for-langs
  (h/load-code-and-locs "(ns aaa (:require [bbb :as b] [ccc :as c]))" (h/file-uri "file:///aaa.clj"))
  (h/load-code-and-locs "(ns bbb (:require [ccc :as c]))" (h/file-uri "file:///bbb.clj"))
  (h/load-code-and-locs "(ns ccc)" (h/file-uri "file:///ccc.clj"))
  (h/load-code-and-locs "(ns jjj (:require [kkk :as k] [lll :as l]))" (h/file-uri "file:///aaa.cljs"))
  (h/load-code-and-locs "(ns kkk (:require [lll :as l]))" (h/file-uri "file:///bbb.cljs"))
  (h/load-code-and-locs "(ns lll)" (h/file-uri "file:///ccc.cljs"))
  (is (= '#{{:alias c :to ccc}
            {:alias b :to bbb}}
         (dep-graph/ns-aliases-for-langs (h/db) #{:clj})))
  (is (= '#{{:alias k :to kkk}
            {:alias l :to lll}}
         (dep-graph/ns-aliases-for-langs (h/db) #{:cljs})))
  (is (= '#{{:alias c :to ccc}
            {:alias b :to bbb}
            {:alias k :to kkk}
            {:alias l :to lll}}
         (dep-graph/ns-aliases-for-langs (h/db) #{:clj :cljs}))))

(deftest ns-names-for-langs
  (h/load-code-and-locs "(ns aaa)" (h/file-uri "file:///aaa.clj"))
  (h/load-code-and-locs "(ns bbb)" (h/file-uri "file:///bbb.clj"))
  (h/load-code-and-locs "(ns ccc)" (h/file-uri "file:///ccc.clj"))
  (h/load-code-and-locs "(ns jjj)" (h/file-uri "file:///aaa.cljs"))
  (h/load-code-and-locs "(ns kkk)" (h/file-uri "file:///bbb.cljs"))
  (h/load-code-and-locs "(ns lll)" (h/file-uri "file:///ccc.cljs"))
  (is (= '#{aaa ccc bbb}
         (dep-graph/ns-names-for-langs (h/db) #{:clj})))
  (is (= '#{jjj kkk lll}
         (dep-graph/ns-names-for-langs (h/db) #{:cljs})))
  (is (= '#{aaa ccc bbb
            jjj kkk lll}
         (dep-graph/ns-names-for-langs (h/db) #{:clj :cljs}))))

(deftest ns-names-for-file
  (h/load-code-and-locs "(ns aaa) (ns ccc)" (h/file-uri "file:///aaa.clj"))
  (h/load-code-and-locs "(ns bbb)" (h/file-uri "file:///bbb.clj"))
  (is (= '[aaa ccc]
         (dep-graph/ns-names-for-uri (h/db) (h/file-uri "file:///aaa.clj")))))

(deftest ns-names
  (h/load-code-and-locs "(ns aaa) (ns ccc)" (h/file-uri "file:///aaa.clj"))
  (h/load-code-and-locs "(ns bbb)" (h/file-uri "file:///bbb.clj"))
  (h/load-code-and-locs "(ns jjj)" (h/file-uri "file:///jjj.cljs"))
  (is (= '#{aaa bbb ccc jjj clojure.core cljs.core}
         (dep-graph/ns-names (h/db)))))

(deftest internal-ns-names
  (h/load-code-and-locs "(ns aaa)" (h/file-uri "file:///aaa.clj"))
  (h/load-code-and-locs "(ns bbb)" (h/file-uri "file:///bbb.clj"))
  (h/load-code-and-locs "(ns some)" (h/file-uri "jar:file:///some.jar!/some-file.clj"))
  (is (= '#{aaa bbb} (dep-graph/internal-ns-names (h/db)))))

(ns clojure-lsp.dep-graph-test
  (:require
   [clojure-lsp.db :as db]
   [clojure-lsp.dep-graph :as dep-graph]
   [clojure-lsp.feature.file-management :as f.file-management]
   [clojure-lsp.shared :as shared]
   [clojure-lsp.test-helper :as h]
   [clojure.test :refer [are deftest is testing]]))

(h/reset-db-after-test)

(defn load-code [file & lines]
  (h/load-code-and-locs
    (apply h/code lines)
    (h/file-uri (str "file://" file))))

(deftest maintaining-dep-graph
  (testing "initial external analysis"
    (h/clean-db!)
    (h/load-code-and-locs (h/code "(ns xxx"
                                  "  (:require [xxx.yyy]))")
                          (h/file-uri "jar:file:///some.jar!/xxx.clj"))
    (h/load-code-and-locs "(ns xxx.yyy)"
                          (h/file-uri "jar:file:///some.jar!/xxx/yyy.clj"))
    (is (= '{xxx {:dependencies {xxx.yyy 1}
                  :filenames #{"/some.jar:xxx.clj"}
                  :internal? false}
             xxx.yyy {:dependents {xxx 1}
                      :aliases {nil 1}
                      :from-internal? false
                      :from-langs {:clj 1}
                      :filenames #{"/some.jar:xxx/yyy.clj"}
                      :internal? false}}
           (:dep-graph @db/db*)))
    (is (= '{"/some.jar:xxx.clj"     {:internal? false, :langs #{:clj}, :namespaces #{xxx}},
             "/some.jar:xxx/yyy.clj" {:internal? false, :langs #{:clj}, :namespaces #{xxx.yyy}}}
           (:file-meta @db/db*))))
  (testing "initial internal analysis"
    (h/clean-db!)
    (load-code "/aaa.clj"
               "(ns aaa"
               " (:require [bbb :as b]"
               "           [ccc :as c]))")
    (load-code "/bbb.clj"
               "(ns bbb"
               " (:require [ccc :as c]))")
    (load-code "/ccc.clj"
               "(ns ccc)")
    (is (= '{aaa {:dependencies {bbb 1, ccc 1}
                  :filenames #{"/aaa.clj"}
                  :internal? true}
             bbb {:dependents {aaa 1}
                  :aliases {b 1}
                  :from-internal? true
                  :from-langs {:clj 1}
                  :dependencies {ccc 1}
                  :filenames #{"/bbb.clj"}
                  :internal? true}
             ccc {:dependents {aaa 1, bbb 1}
                  :aliases {c 2}
                  :from-internal? true
                  :from-langs {:clj 2}
                  :filenames #{"/ccc.clj"}
                  :internal? true}}
           (:dep-graph @db/db*)))
    (is (= '{"/aaa.clj" {:internal? true, :langs #{:clj}, :namespaces #{aaa}}
             "/bbb.clj" {:internal? true, :langs #{:clj}, :namespaces #{bbb}}
             "/ccc.clj" {:internal? true, :langs #{:clj}, :namespaces #{ccc}}}
           (:file-meta @db/db*))))
  (testing "extending initial external analysis with internal analysis"
    (h/clean-db!)
    (h/load-code-and-locs (h/code "(ns xxx"
                                  "  (:require [xxx.yyy]))")
                          (h/file-uri "jar:file:///some.jar!/xxx.clj"))
    (h/load-code-and-locs "(ns xxx.yyy)"
                          (h/file-uri "jar:file:///some.jar!/xxx/yyy.clj"))
    (let [db @db/db*
          xxx (get-in db [:dep-graph 'xxx])
          xxx-yyy (get-in db [:dep-graph 'xxx.yyy])]
      (is (not (:internal? xxx)))
      (is (not (:from-internal? xxx)))
      (is (not (:internal? xxx-yyy)))
      (is (not (:from-internal? xxx-yyy))))
    (load-code "/aaa.clj"
               "(ns aaa"
               " (:require [xxx :as x]))")
    (let [db @db/db*
          xxx (get-in db [:dep-graph 'xxx])
          xxx-yyy (get-in db [:dep-graph 'xxx.yyy])]
      (is (not (:internal? xxx)))
      (is (:from-internal? xxx)) ;; <-- this is the change
      (is (not (:internal? xxx-yyy)))
      (is (not (:from-internal? xxx-yyy)))))
  (testing "adding dependency"
    (h/clean-db!)
    (load-code "/bbb.clj"
               "(ns bbb)")
    (load-code "/aaa.clj"
               "(ns aaa)")
    (let [db @db/db*]
      (is (empty? (get-in db [:dep-graph 'aaa :dependencies])))
      (is (empty? (get-in db [:dep-graph 'bbb :dependents]))))
    (load-code "/aaa.clj"
               "(ns aaa"
               " (:require [bbb :as b]))")
    (let [db @db/db*]
      (is (get-in db [:dep-graph 'aaa :dependencies 'bbb]))
      (is (get-in db [:dep-graph 'bbb :dependents 'aaa]))))
  (testing "removing dependency"
    (h/clean-db!)
    (load-code "/bbb.clj"
               "(ns bbb)")
    (load-code "/aaa.clj"
               "(ns aaa"
               " (:require [bbb :as b]))")
    (let [db @db/db*]
      (is (get-in db [:dep-graph 'aaa :dependencies 'bbb]))
      (is (get-in db [:dep-graph 'bbb :dependents 'aaa])))
    (load-code "/aaa.clj"
               "(ns aaa)")
    (let [db @db/db*]
      (is (empty? (get-in db [:dep-graph 'aaa :dependencies])))
      (is (empty? (get-in db [:dep-graph 'bbb :dependents])))))
  (testing "removing duplicate dependency"
    (h/clean-db!)
    (load-code "/bbb.clj"
               "(ns bbb)")
    (load-code "/aaa.clj"
               "(ns aaa"
               " (:require [bbb :as b]"
               "           [bbb :as b2]))")
    (let [db @db/db*]
      (is (get-in db [:dep-graph 'aaa :dependencies 'bbb]))
      (is (get-in db [:dep-graph 'bbb :dependents 'aaa]))
      (is (= '{b 1, b2 1} (get-in db [:dep-graph 'bbb :aliases]))))
    (load-code "/aaa.clj"
               "(ns aaa"
               " (:require [bbb :as b]))")
    (let [db @db/db*]
      (is (get-in db [:dep-graph 'aaa :dependencies 'bbb]))
      (is (get-in db [:dep-graph 'bbb :dependents 'aaa]))
      (is (= '{b 1} (get-in db [:dep-graph 'bbb :aliases])))))
  (testing "deleting file"
    (h/clean-db!)
    (load-code "/bbb.clj"
               "(ns bbb)")
    (load-code "/aaa.clj"
               "(ns aaa"
               " (:require [bbb :as b]))")
    (load-code "/ccc.clj"
               "(ns ccc"
               " (:require [aaa :as a]))")
    (let [db @db/db*]
      (is (seq (get-in db [:dep-graph 'aaa :dependencies])))
      (is (= '{ccc 1} (get-in db [:dep-graph 'aaa :dependents])))
      (is (seq (get-in db [:dep-graph 'aaa :filenames])))
      (is (seq (get-in db [:dep-graph 'bbb :dependents])))
      (is (not (nil? (get-in db [:file-meta "/aaa.clj"])))))
    #_(alter-var-root #'db/diagnostics-chan (constantly (async/chan 1)))
    (with-redefs [shared/file-exists? (constantly false)]
      (f.file-management/did-close "file:///aaa.clj" db/db*))
    (let [db @db/db*]
      (is (empty? (get-in db [:dep-graph 'aaa :dependencies])))
      (is (= '{ccc 1} (get-in db [:dep-graph 'aaa :dependents]))) ;; <-- no change, because ccc stil depends on aaa, even though aaa is now undefined
      (is (empty? (get-in db [:dep-graph 'aaa :filenames])))
      (is (empty? (get-in db [:dep-graph 'bbb :dependents])))
      (is (nil? (get-in db [:file-meta "/aaa.clj"])))))
  (testing "in implicit user ns"
    (h/clean-db!)
    (load-code "/scratch.clj"
               "(def x 1)")
    (let [db @db/db*]
      (is (= #{"/scratch.clj"} (get-in db [:dep-graph 'user :filenames])))
      (is (= '#{user} (get-in db [:file-meta "/scratch.clj" :namespaces])))))
  (testing "with implicit dependency on clojure.core"
    (h/clean-db!)
    (load-code "/aaa.clj"
               "(ns aaa)"
               "(def x 1)")
    (let [db @db/db*]
      (is (get-in db [:dep-graph 'aaa :dependencies 'clojure.core]))
      (is (get-in db [:dep-graph 'clojure.core :dependents 'aaa]))))
  (testing "with implicit dependency on cljs.core"
    (h/clean-db!)
    (load-code "/aaa.cljs"
               "(ns aaa)"
               "(def x 1)")
    (let [db @db/db*]
      (is (get-in db [:dep-graph 'aaa :dependencies 'cljs.core]))
      (is (get-in db [:dep-graph 'cljs.core :dependents 'aaa])))))

(deftest file-filtering
  (h/clean-db!)
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
    (let [db @db/db*]
      (are [expected namespace]
           (= expected (dep-graph/ns-filenames db namespace))
        #{"/aaa.clj"} 'aaa
        #{"/bbb.clj"} 'bbb
        #{"/ccc.clj"} 'ccc)
      (are [expected namespace]
           (= expected (dep-graph/ns-dependencies-filenames db namespace))
        #{"/bbb.clj" "/ccc.clj"} 'aaa
        #{"/ccc.clj"}            'bbb
        #{}                      'ccc)
      (are [expected namespace]
           (= expected (dep-graph/ns-dependents-filenames db namespace))
        #{}                      'aaa
        #{"/aaa.clj"}            'bbb
        #{"/aaa.clj" "/bbb.clj"} 'ccc)
      (are [expected namespace]
           (= expected (dep-graph/ns-and-dependents-filenames db namespace))
        #{"/aaa.clj"}                       'aaa
        #{"/aaa.clj" "/bbb.clj"}            'bbb
        #{"/aaa.clj" "/bbb.clj" "/ccc.clj"} 'ccc)
      (is (= #{"/aaa.clj" "/bbb.clj"}
             (dep-graph/nses-filenames db '#{aaa bbb})))
      (is (= #{"/aaa.clj" "/bbb.clj" "/ccc.clj"}
             (dep-graph/nses-and-dependents-filenames db '#{bbb ccc})))))
  (testing "external namespaces"
    (h/clean-db!)
    (load-code "/aaa.clj" "(ns aaa)")
    (h/load-code-and-locs "(ns bbb)" (h/file-uri "jar:file:///some.jar!/bbb.clj"))
    (let [db @db/db*]
      (is (= ["/aaa.clj"] (dep-graph/internal-filenames db)))))
  (testing "namespaces defined internally and externally"
    (h/clean-db!)
    (load-code "/aaa.clj" "(ns aaa)")
    (h/load-code-and-locs "(ns aaa)" (h/file-uri "jar:file:///some.jar!/aaa.clj"))
    (let [db @db/db*]
      (is (= #{"/aaa.clj" "/some.jar:aaa.clj"} (dep-graph/ns-filenames db 'aaa)))
      (is (= ["/aaa.clj"] (dep-graph/ns-internal-filenames db 'aaa)))
      (is (= ["/aaa.clj"] (dep-graph/internal-filenames db)))))
  (testing "file with multiple namespaces"
    (h/clean-db!)
    (load-code "/aaa.clj"
               "(ns aaa)"
               "(ns aaa2)")
    (let [db @db/db*]
      (is (= #{"/aaa.clj"} (dep-graph/ns-filenames db 'aaa)))
      (is (= #{"/aaa.clj"} (dep-graph/ns-filenames db 'aaa2)))
      (is (= '#{aaa aaa2} (get-in db [:file-meta "/aaa.clj" :namespaces])))))
  (testing "namespace with multiple files"
    (h/clean-db!)
    (load-code "/aaa.clj" "(ns aaa)")
    (load-code "/also/aaa.clj" "(ns aaa)")
    (let [db @db/db*]
      (is (= #{"/aaa.clj" "/also/aaa.clj"} (dep-graph/ns-filenames db 'aaa)))
      (is (= '#{aaa} (get-in db [:file-meta "/aaa.clj" :namespaces])))
      (is (= '#{aaa} (get-in db [:file-meta "/also/aaa.clj" :namespaces]))))))

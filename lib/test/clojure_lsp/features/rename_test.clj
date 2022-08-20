(ns clojure-lsp.features.rename-test
  (:require
   [clojure-lsp.feature.rename :as f.rename]
   [clojure-lsp.test-helper :as h]
   [clojure.test :refer [deftest is testing]]))

(deftest rename-simple-keywords
  (let [components (h/make-components)
        [a-start _a-stop
         a-binding-start a-binding-stop
         a-local-usage-start a-local-usage-stop] (h/load-code
                                                   "|:a| (let [{:keys [:|a|]} {}] |a|)"
                                                   (h/file-uri "file:///a.cljc") components)
        db (h/db components)]
    (testing "should not rename plain keywords"
      (let [[row col] a-start
            result (f.rename/rename-from-position (h/file-uri "file:///a.cljc") ":b" row col db)]
        (is (= {:error {:code :invalid-params
                        :message "Can't rename, only namespaced keywords can be renamed."}}
               result))))

    (testing "should rename local in destructure but not keywords"
      (let [[row col] a-binding-start
            changes (:changes (f.rename/rename-from-position (h/file-uri "file:///a.cljc") ":b" row col db))]
        (is (= {(h/file-uri "file:///a.cljc")
                [{:new-text "b" :range (h/->range a-binding-start a-binding-stop)}
                 {:new-text "b" :range (h/->range a-local-usage-start a-local-usage-stop)}]}
               changes))))))

(deftest rename-namespaced-keywords
  (let [components (h/make-components)
        [a-start a-stop] (h/load-code
                           "(ns a) |:hello/foo|"
                           (h/file-uri "file:///a.cljc") components)
        [b-start b-stop] (h/load-code
                           "(ns b) |:hello/foo|"
                           (h/file-uri "file:///b.cljc") components)
        db (h/db components)]
    (testing "renaming only the name"
      (let [[row col] a-start
            changes (:changes (f.rename/rename-from-position (h/file-uri "file:///a.cljc") ":hello/bar" row col db))]
        (is (= {(h/file-uri "file:///a.cljc")
                [{:new-text ":hello/bar" :range (h/->range a-start a-stop)}]
                (h/file-uri "file:///b.cljc")
                [{:new-text ":hello/bar" :range (h/->range b-start b-stop)}]}
               changes))))
    (testing "renaming only the namespace"
      (let [[row col] a-start
            changes (:changes (f.rename/rename-from-position (h/file-uri "file:///a.cljc") ":bye/foo" row col db))]
        (is (= {(h/file-uri "file:///a.cljc")
                [{:new-text ":bye/foo" :range (h/->range a-start a-stop)}]
                (h/file-uri "file:///b.cljc")
                [{:new-text ":bye/foo" :range (h/->range b-start b-stop)}]}
               changes))))
    (testing "renaming namespace and name"
      (let [[row col] a-start
            changes (:changes (f.rename/rename-from-position (h/file-uri "file:///a.cljc") ":bye/bar" row col db))]
        (is (= {(h/file-uri "file:///a.cljc")
                [{:new-text ":bye/bar" :range (h/->range a-start a-stop)}]
                (h/file-uri "file:///b.cljc")
                [{:new-text ":bye/bar" :range (h/->range b-start b-stop)}]}
               changes))))))

(deftest rename-destructuring-keywords
  (let [components (h/make-components)
        [a-start a-stop
         a-binding-start a-binding-stop
         a-usage-start a-usage-stop] (h/load-code
                                       "|:a/b| (let [{:keys [|:a/b|]} {}] |b|)"
                                       (h/file-uri "file:///a.cljc") components)
        [b-start b-stop
         b-binding-start b-binding-stop
         b-usage-start b-usage-stop] (h/load-code
                                       "|:c/d| (let [{:keys [|c/d|]} {}] |d|)"
                                       (h/file-uri "file:///b.cljc") components)
        [c-start c-stop
         c-binding-start c-binding-stop
         c-usage-start c-usage-stop] (h/load-code
                                       "|:e/f| (let [{:e/keys [|f|]} {}] |f|)"
                                       (h/file-uri "file:///c.cljc") components)
        db (h/db components)]
    (testing "should rename local in destructure with ':' and keywords if namespaced"
      (let [[row col] a-start
            changes (:changes (f.rename/rename-from-position (h/file-uri "file:///a.cljc") ":a/c" row col db))]
        (is (= {(h/file-uri "file:///a.cljc")
                [{:new-text ":a/c" :range (h/->range a-start a-stop)}
                 {:new-text ":a/c" :range (h/->range a-binding-start a-binding-stop)}
                 {:new-text "c" :range (h/->range a-usage-start a-usage-stop)}]}
               changes))))
    (testing "should rename local in destructure without ':' and keywords if namespaced"
      (let [[row col] b-start
            changes (:changes (f.rename/rename-from-position (h/file-uri "file:///b.cljc") ":c/e" row col db))]
        (is (= {(h/file-uri "file:///b.cljc")
                [{:new-text ":c/e" :range (h/->range b-start b-stop)}
                 {:new-text "c/e" :range (h/->range b-binding-start b-binding-stop)}
                 {:new-text "e" :range (h/->range b-usage-start b-usage-stop)}]}
               changes))))
    (testing "should rename local in destructure with namespace on :keys"
      (let [[row col] c-start
            changes (:changes (f.rename/rename-from-position (h/file-uri "file:///c.cljc") ":e/g" row col db))]
        (is (= {(h/file-uri "file:///c.cljc")
                [{:new-text ":e/g" :range (h/->range c-start c-stop)}
                 {:new-text "g" :range (h/->range c-binding-start c-binding-stop)}
                 {:new-text "g" :range (h/->range c-usage-start c-usage-stop)}]}
               changes))))))

(deftest rename-keywords-corner-cases
  (let [components (h/make-components)
        [a-b-start a-b-stop
         b-ns-start b-ns-stop]
        (h/load-code
          (h/code ":a"
                  "::a"
                  "|:a/b|"
                  "#:a{|:b| 1"
                  "    :c/d 2"
                  "    :_/e 3"
                  "    ::f 4}")
          (h/file-uri "file:///a.cljc") components)
        [h1-start h1-stop
         h2-start h2-stop
         _h3-start _h3-stop]
        (h/load-code
          (h/code "|::hello-world|"
                  "|::hello-world|"
                  "|:hello/world|")
          (h/file-uri "file:///b.cljc") components)
        db (h/db components)]
    (testing "renaming keywords renames correctly namespaced maps as well"
      (let [[row col] a-b-start
            changes (:changes (f.rename/rename-from-position (h/file-uri "file:///a.cljc") ":a/g" row col db))]
        (is (= {(h/file-uri "file:///a.cljc") [{:new-text ":a/g" :range (h/->range a-b-start a-b-stop)}
                                               {:new-text ":g" :range (h/->range b-ns-start b-ns-stop)}]}
               changes))))
    (testing "renaming from aliased namespace to namespace"
      (let [[row col] h1-start
            changes (:changes (f.rename/rename-from-position (h/file-uri "file:///b.cljc") ":hello/world" row col db))]
        (is (= {(h/file-uri "file:///b.cljc") [{:new-text ":hello/world" :range (h/->range h1-start h1-stop)}
                                               {:new-text ":hello/world" :range (h/->range h2-start h2-stop)}]}
               changes))))
    #_(testing "renaming from namespace to aliased namespace"
        (let [[row col] h3-start
              changes (:changes (f.rename/rename (h/file-uri "file:///b.cljc") "::hello-world" row col db/db))]
          (is (= {(h/file-uri "file:///b.cljc") [{:new-text "::hello-world" :range (h/->range h3-start h3-stop)}]}
                 changes))))))

(deftest rename-namespaces
  (testing "when client has valid source-paths but no document-changes capability"
    (let [components (h/make-components {:project-root-uri (h/file-uri "file:///my-project")
                                         :settings {:source-paths #{(h/file-path "/my-project/src") (h/file-path "/my-project/test")}}
                                         :client-capabilities {:workspace {:workspace-edit {:document-changes false}}}})
          [[row col]] (h/load-code "(ns |foo.bar-baz)" (h/file-uri "file:///my-project/src/foo/bar_baz.clj") components)
          db (h/db components)]
      (h/assert-submap
        {:error {:code :invalid-params
                 :message "Can't rename namespace, client does not support file renames."}}
        (f.rename/rename-from-position (h/file-uri "file:///my-project/src/foo/bar_baz.clj") "foo.baz-qux" row col db))))
  (testing "when client has document-changes capability but no valid source-paths"
    (let [components (h/make-components {:project-root-uri (h/file-uri "file:///my-project")
                                         :settings {:source-paths #{(h/file-path "/my-project/bla")}}
                                         :client-capabilities {:workspace {:workspace-edit {:document-changes true}}}})
          [[row col]] (h/load-code "(ns |foo.bar-baz)" (h/file-uri "file:///my-project/src/foo/bar_baz.clj") components)
          db (h/db components)]
      (h/assert-submap
        {:error {:code :invalid-params
                 :message "Can't rename namespace, invalid source-paths. Are project :source-paths configured correctly?"}}
        (f.rename/rename-from-position (h/file-uri "file:///my-project/src/foo/bar_baz.clj") "foo.baz-qux" row col db))))
  (testing "when source-paths are valid and client capabilities has document-changes"
    (let [components (h/make-components {:project-root-uri (h/file-uri "file:///my-project")
                                         :settings {:source-paths #{(h/file-path "/my-project/src") (h/file-path "/my-project/test")}}
                                         :client-capabilities {:workspace {:workspace-edit {:document-changes true}}}})
          [ns-start ns-end] (h/load-code "(ns |foo.bar-baz|)" (h/file-uri "file:///my-project/src/foo/bar_baz.clj") components)
          [row col] ns-start
          db (h/db components)]
      (is (= {:document-changes
              [{:text-document {:version 0
                                :uri (h/file-uri "file:///my-project/src/foo/bar_baz.clj")}
                :edits [{:range (h/->range ns-start ns-end)
                         :new-text "foo.baz-qux"}]}
               {:kind "rename"
                :old-uri (h/file-uri "file:///my-project/src/foo/bar_baz.clj")
                :new-uri (h/file-uri "file:///my-project/src/foo/baz_qux.clj")}]}
             (f.rename/rename-from-position (h/file-uri "file:///my-project/src/foo/bar_baz.clj") "foo.baz-qux" row col db))))))

(deftest rename-defrecord
  (let [components (h/make-components)
        [def-start-pos def-end-pos
         use1-start-pos use1-end-pos
         use2-start-pos use2-end-pos
         use3-start-pos use3-end-pos
         use4-start-pos use4-end-pos] (h/load-code (h/code "(defrecord |Foo| [a])"
                                                           "(|Foo|. 1)"
                                                           "(|->Foo| 1)"
                                                           "(|map->Foo| {:a 1})"
                                                           "|Foo|") h/default-uri components)
        [start-row start-col] def-start-pos
        result (:changes (f.rename/rename-from-position h/default-uri "Bar" start-row start-col (h/db components)))]
    (is (= {h/default-uri [{:new-text "Bar" :range (h/->range def-start-pos def-end-pos)}
                           {:new-text "Bar" :range (h/->range use1-start-pos use1-end-pos)}
                           {:new-text "->Bar" :range (h/->range use2-start-pos use2-end-pos)}
                           {:new-text "map->Bar" :range (h/->range use3-start-pos use3-end-pos)}
                           {:new-text "Bar" :range (h/->range use4-start-pos use4-end-pos)}]}
           result))))

(deftest prepare-rename
  (testing "rename local var"
    (let [components (h/make-components)
          [local-start local-end] (h/load-code "(let [|a| 1] a)" h/default-uri components)
          [row col] local-start
          db (h/db components)
          result (f.rename/prepare-rename h/default-uri row col db)]
      (is (= (h/->range local-start local-end)
             result))))
  (testing "should not rename when on unnamed element"
    (let [components (h/make-components)
          [[row col]] (h/load-code "|[]" h/default-uri components)
          db (h/db components)
          result (f.rename/prepare-rename h/default-uri row col db)]
      (is (= {:error {:code :invalid-params
                      :message "Can't rename, no element found."}}
             result))))
  (testing "should not rename plain keywords"
    (let [components (h/make-components)
          [[row col]] (h/load-code "|:a" h/default-uri components)
          db (h/db components)
          result (f.rename/prepare-rename h/default-uri row col db)]
      (is (= {:error {:code :invalid-params
                      :message "Can't rename, only namespaced keywords can be renamed."}}
             result))))
  (testing "when client has valid source-paths but no document-changes capability"
    (let [components (h/make-components {:project-root-uri (h/file-uri "file:///")
                                         :settings {:source-paths #{(h/file-path "/")}}
                                         :client-capabilities {:workspace {:workspace-edit {:document-changes false}}}})
          [[row col]] (h/load-code "(ns |foo.bar-baz)" h/default-uri components)
          db (h/db components)]
      (h/assert-submap
        {:error {:code :invalid-params
                 :message "Can't rename namespace, client does not support file renames."}}
        (f.rename/prepare-rename h/default-uri row col db))))
  (testing "when client has document-changes capability but no valid source-paths"
    (let [components (h/make-components {:project-root-uri (h/file-uri "file:///")
                                         :settings {:source-paths #{(h/file-path "/bla")}}
                                         :client-capabilities {:workspace {:workspace-edit {:document-changes true}}}})
          [[row col]] (h/load-code "(ns |foo.bar-baz)" h/default-uri components)
          db (h/db components)]
      (h/assert-submap
        {:error {:code :invalid-params
                 :message "Can't rename namespace, invalid source-paths. Are project :source-paths configured correctly?"}}
        (f.rename/prepare-rename h/default-uri row col db)))))

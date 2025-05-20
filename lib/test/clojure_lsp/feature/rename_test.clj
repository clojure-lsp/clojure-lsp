(ns clojure-lsp.feature.rename-test
  (:require
   [clojure-lsp.feature.rename :as f.rename]
   [clojure-lsp.shared :as shared]
   [clojure-lsp.test-helper.internal :as h]
   [clojure.test :refer [deftest is testing]]))

(h/reset-components-before-test)

(deftest rename-simple-keywords
  (let [[a-start _a-stop
         a-binding-start a-binding-stop
         a-local-usage-start a-local-usage-stop] (h/load-code-and-locs
                                                   "|:a| (let [{:keys [:|a|]} {}] |a|)"
                                                   (h/file-uri "file:///a.cljc"))]
    (testing "should not rename plain keywords"
      (let [[row col] a-start
            result (f.rename/rename-from-position (h/file-uri "file:///a.cljc") ":b" row col (h/db))]
        (is (= {:error {:code :invalid-params
                        :message "Can't rename - only namespaced keywords can be renamed."}}
               result))))

    (testing "should rename local in destructure but not keywords"
      (let [[row col] a-binding-start
            changes (:changes (f.rename/rename-from-position (h/file-uri "file:///a.cljc") ":b" row col (h/db)))]
        (is (= {(h/file-uri "file:///a.cljc")
                [{:new-text "b" :range (h/->range a-binding-start a-binding-stop)}
                 {:new-text "b" :range (h/->range a-local-usage-start a-local-usage-stop)}]}
               changes))))))

(deftest rename-namespaced-keywords
  (let [[a-start a-stop] (h/load-code-and-locs
                           "(ns a) |:hello/foo|"
                           (h/file-uri "file:///a.cljc"))
        [b-start b-stop] (h/load-code-and-locs
                           "(ns b) |:hello/foo|"
                           (h/file-uri "file:///b.cljc"))]
    (testing "renaming only the name"
      (let [[row col] a-start
            changes (:changes (f.rename/rename-from-position (h/file-uri "file:///a.cljc") ":hello/bar" row col (h/db)))]
        (is (= {(h/file-uri "file:///a.cljc")
                [{:new-text ":hello/bar" :range (h/->range a-start a-stop)}]
                (h/file-uri "file:///b.cljc")
                [{:new-text ":hello/bar" :range (h/->range b-start b-stop)}]}
               changes))))
    (testing "renaming only the namespace"
      (let [[row col] a-start
            changes (:changes (f.rename/rename-from-position (h/file-uri "file:///a.cljc") ":bye/foo" row col (h/db)))]
        (is (= {(h/file-uri "file:///a.cljc")
                [{:new-text ":bye/foo" :range (h/->range a-start a-stop)}]
                (h/file-uri "file:///b.cljc")
                [{:new-text ":bye/foo" :range (h/->range b-start b-stop)}]}
               changes))))
    (testing "renaming namespace and name"
      (let [[row col] a-start
            changes (:changes (f.rename/rename-from-position (h/file-uri "file:///a.cljc") ":bye/bar" row col (h/db)))]
        (is (= {(h/file-uri "file:///a.cljc")
                [{:new-text ":bye/bar" :range (h/->range a-start a-stop)}]
                (h/file-uri "file:///b.cljc")
                [{:new-text ":bye/bar" :range (h/->range b-start b-stop)}]}
               changes))))))

(deftest rename-destructuring-keywords
  (let [[a-start a-stop
         a-binding-start a-binding-stop
         a-usage-start a-usage-stop] (h/load-code-and-locs
                                       "|:a/b| (let [{:keys [|:a/b|]} {}] |b|)"
                                       (h/file-uri "file:///a.cljc"))
        [b-start b-stop
         b-binding-start b-binding-stop
         b-usage-start b-usage-stop] (h/load-code-and-locs
                                       "|:c/d| (let [{:keys [|c/d|]} {}] |d|)"
                                       (h/file-uri "file:///b.cljc"))
        [c-start c-stop
         c-binding-start c-binding-stop
         c-usage-start c-usage-stop] (h/load-code-and-locs
                                       "|:e/f| (let [{:e/keys [|f|]} {}] |f|)"
                                       (h/file-uri "file:///c.cljc"))]
    (testing "should rename local in destructure with ':' and keywords if namespaced"
      (let [[row col] a-start
            changes (:changes (f.rename/rename-from-position (h/file-uri "file:///a.cljc") ":a/c" row col (h/db)))]
        (is (= {(h/file-uri "file:///a.cljc")
                [{:new-text ":a/c" :range (h/->range a-start a-stop)}
                 {:new-text ":a/c" :range (h/->range a-binding-start a-binding-stop)}
                 {:new-text "c" :range (h/->range a-usage-start a-usage-stop)}]}
               changes))))
    (testing "should rename local in destructure without ':' and keywords if namespaced"
      (let [[row col] b-start
            changes (:changes (f.rename/rename-from-position (h/file-uri "file:///b.cljc") ":c/e" row col (h/db)))]
        (is (= {(h/file-uri "file:///b.cljc")
                [{:new-text ":c/e" :range (h/->range b-start b-stop)}
                 {:new-text "c/e" :range (h/->range b-binding-start b-binding-stop)}
                 {:new-text "e" :range (h/->range b-usage-start b-usage-stop)}]}
               changes))))
    (testing "should rename local in destructure with namespace on :keys"
      (let [[row col] c-start
            changes (:changes (f.rename/rename-from-position (h/file-uri "file:///c.cljc") ":e/g" row col (h/db)))]
        (is (= {(h/file-uri "file:///c.cljc")
                [{:new-text ":e/g" :range (h/->range c-start c-stop)}
                 {:new-text "g" :range (h/->range c-binding-start c-binding-stop)}
                 {:new-text "g" :range (h/->range c-usage-start c-usage-stop)}]}
               changes))))))

(deftest rename-keywords-corner-cases
  (let [[a-b-start a-b-stop
         b-ns-start b-ns-stop]
        (h/load-code-and-locs
          (h/code ":a"
                  "::a"
                  "|:a/b|"
                  "#:a{|:b| 1"
                  "    :c/d 2"
                  "    :_/e 3"
                  "    ::f 4}")
          (h/file-uri "file:///a.cljc"))
        [h1-start h1-stop
         h2-start h2-stop
         _h3-start _h3-stop]
        (h/load-code-and-locs
          (h/code "|::hello-world|"
                  "|::hello-world|"
                  "|:hello/world|")
          (h/file-uri "file:///b.cljc"))]
    (testing "renaming keywords renames correctly namespaced maps as well"
      (let [[row col] a-b-start
            changes (:changes (f.rename/rename-from-position (h/file-uri "file:///a.cljc") ":a/g" row col (h/db)))]
        (is (= {(h/file-uri "file:///a.cljc") [{:new-text ":a/g" :range (h/->range a-b-start a-b-stop)}
                                               {:new-text ":g" :range (h/->range b-ns-start b-ns-stop)}]}
               changes))))
    (testing "renaming from aliased namespace to namespace"
      (let [[row col] h1-start
            changes (:changes (f.rename/rename-from-position (h/file-uri "file:///b.cljc") ":hello/world" row col (h/db)))]
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
    (h/reset-components!)
    (swap! (h/db*) shared/deep-merge
           {:project-root-uri (h/file-uri "file:///my-project")
            :settings {:source-paths #{(h/file-path "/my-project/src") (h/file-path "/my-project/test")}}
            :client-capabilities {:workspace {:workspace-edit {:document-changes false}}}})
    (h/load-code-and-locs "(ns foo.bar-baz)" (h/file-uri "file:///my-project/src/foo/bar_baz.clj"))
    (h/assert-submap
      {:error {:code :invalid-params
               :message "Can't rename - client does not support file renames."}}
      (f.rename/rename-from-position (h/file-uri "file:///my-project/src/foo/bar_baz.clj") "foo.baz-qux" 1 5 (h/db))))
  (testing "when client has document-changes capability but no valid source-paths"
    (h/reset-components!)
    (swap! (h/db*) shared/deep-merge
           {:project-root-uri (h/file-uri "file:///my-project")
            :settings {:source-paths #{(h/file-path "/my-project/bla")}}
            :client-capabilities {:workspace {:workspace-edit {:document-changes true}}}})
    (h/load-code-and-locs "(ns foo.bar-baz)" (h/file-uri "file:///my-project/src/foo/bar_baz.clj"))
    (h/assert-submap
      {:error {:code :invalid-params
               :message "Can't rename - invalid source-paths. Are project :source-paths configured correctly?"}}
      (f.rename/rename-from-position (h/file-uri "file:///my-project/src/foo/bar_baz.clj") "foo.baz-qux" 1 5 (h/db))))
  (testing "when namespace is defined in multiple files"
    (h/reset-components!)
    (swap! (h/db*) shared/deep-merge
           {:project-root-uri (h/file-uri "file:///my-project")
            :settings {:source-paths #{(h/file-path "/my-project/src") (h/file-path "/my-project/test")}}
            :client-capabilities {:workspace {:workspace-edit {:document-changes true}}}})
    (h/load-code-and-locs "(ns foo.bar-baz)" (h/file-uri "file:///my-project/src/foo/bar_baz.clj"))
    (h/load-code-and-locs "(ns foo.bar-baz)" (h/file-uri "file:///my-project/test/foo/bar_baz.clj"))
    (h/assert-submap
      {:error {:code :invalid-params
               :message "Can't rename - namespace is defined in multiple files."}}
      (f.rename/rename-from-position (h/file-uri "file:///my-project/src/foo/bar_baz.clj") "foo.baz-qux" 1 5 (h/db))))
  (testing "should trigger only a rename change and not all references changes"
    (testing "when source-paths are valid and client capabilities has document-changes"
      (h/reset-components!)
      (swap! (h/db*) shared/deep-merge
             {:project-root-uri (h/file-uri "file:///my-project")
              :settings {:source-paths #{(h/file-path "/my-project/src") (h/file-path "/my-project/test")}}
              :client-capabilities {:workspace {:workspace-edit {:document-changes true}}}})
      (h/load-code-and-locs (h/code "(ns foo.bar-baz (:require [clojure.spec.alpha :as s]))"
                                    "(s/def ::foo 1)") (h/file-uri "file:///my-project/src/foo/bar_baz.clj"))
      (h/load-code-and-locs (h/code "(ns foo.qux (:require [foo.bar-baz :as f]))"
                                    ":foo.bar-baz/foo") (h/file-uri "file:///my-project/src/foo/qux.clj"))
      (is (= {:document-changes
              [{:kind "rename"
                :old-uri (h/file-uri "file:///my-project/src/foo/bar_baz.clj")
                :new-uri (h/file-uri "file:///my-project/src/foo/baz_qux.clj")}]}
             (f.rename/rename-from-position (h/file-uri "file:///my-project/src/foo/bar_baz.clj") "foo.baz-qux" 1 5 (h/db)))))
    (testing "renaming ns for cljc files"
      (h/reset-components!)
      (swap! (h/db*) shared/deep-merge
             {:project-root-uri (h/file-uri "file:///my-project")
              :settings {:source-paths #{(h/file-path "/my-project/src") (h/file-path "/my-project/test")}}
              :client-capabilities {:workspace {:workspace-edit {:document-changes true}}}})
      (h/load-code-and-locs (h/code "(ns foo.bar-baz (:require [clojure.spec.alpha :as s]))"
                                    "(s/def ::foo 1)") (h/file-uri "file:///my-project/src/foo/bar_baz.cljc"))
      (h/load-code-and-locs (h/code "(ns foo.qux (:require [foo.bar-baz :as f]))"
                                    ":foo.bar-baz/foo") (h/file-uri "file:///my-project/src/foo/qux.cljc"))
      (is (= {:document-changes
              [{:kind "rename"
                :old-uri (h/file-uri "file:///my-project/src/foo/bar_baz.cljc")
                :new-uri (h/file-uri "file:///my-project/src/foo/baz_qux.cljc")}]}
             (f.rename/rename-from-position (h/file-uri "file:///my-project/src/foo/bar_baz.cljc") "foo.baz-qux" 1 5 (h/db)))))))

(deftest rename-defrecord
  (h/reset-components!)
  (testing "considering all types of defrecord new instance"
    (let [[def-start-pos def-end-pos
           use1-start-pos use1-end-pos
           use2-start-pos use2-end-pos
           use3-start-pos use3-end-pos
           use4-start-pos use4-end-pos] (h/load-code-and-locs (h/code "(defrecord |Foo| [a])"
                                                                      "(|Foo|. 1)"
                                                                      "(|->Foo| 1)"
                                                                      "(|map->Foo| {:a 1})"
                                                                      "|Foo|"))
          [start-row start-col] def-start-pos
          result (:changes (f.rename/rename-from-position h/default-uri "Bar" start-row start-col (h/db)))]
      (is (= {h/default-uri [{:new-text "Bar" :range (h/->range def-start-pos def-end-pos)}
                             {:new-text "Bar" :range (h/->range use1-start-pos use1-end-pos)}
                             {:new-text "->Bar" :range (h/->range use2-start-pos use2-end-pos)}
                             {:new-text "map->Bar" :range (h/->range use3-start-pos use3-end-pos)}
                             {:new-text "Bar" :range (h/->range use4-start-pos use4-end-pos)}]}
             result))))
  (testing "across namespaces"
    (let [[def-start-pos def-end-pos] (h/load-code-and-locs (h/code "(ns a)"
                                                                    "(defrecord |Foo| [])"))
          [usage-start-pos usage-end-pos] (h/load-code-and-locs (h/code "(ns b (:require [a :as foo]))"
                                                                        "(foo/|Foo|)") (h/file-uri "file:///b.clj"))
          [start-row start-col] def-start-pos
          result (:changes (f.rename/rename-from-position h/default-uri "Bar" start-row start-col (h/db)))]
      (is (= {h/default-uri [{:new-text "Bar" :range (h/->range def-start-pos def-end-pos)}]
              (h/file-uri "file:///b.clj") [{:new-text "Bar" :range (h/->range usage-start-pos usage-end-pos)}]}
             result)))))

(deftest rename-alias-from-usages
  (h/reset-components!)
  (let [[def-start-pos def-end-pos] (h/load-code-and-locs (h/code "(ns some.ns)"
                                                                  "(defn |foo| [a] 1)"))
        [other-usage-start-pos other-usage-end-pos] (h/load-code-and-locs
                                                      (h/code "(ns another.ns (:require [some.ns :as other-alias]))"
                                                              "other-alias/|foo|") (h/file-uri "file:///b.clj"))
        [alias-start-pos alias-end-pos
         usage-1-start-pos usage-1-end-pos
         usage-2-start-pos usage-2-end-pos] (h/load-code-and-locs
                                              (h/code "(ns other.ns (:require [some.ns :as |my-alias|]))"
                                                      "|my-alias/foo|"
                                                      "(|my-alias/foo| 1)") (h/file-uri "file:///c.clj"))
        [usage-start-r usage-start-c] usage-2-start-pos
        result (:changes (f.rename/rename-from-position (h/file-uri "file:///c.clj") "your-alias/bar" usage-start-r usage-start-c (h/db)))]
    (h/assert-submap
      {h/default-uri [{:range (h/->range def-start-pos def-end-pos) :new-text "bar"}]
       (h/file-uri "file:///b.clj") [{:range (h/->range other-usage-start-pos other-usage-end-pos) :new-text "bar"}]
       (h/file-uri "file:///c.clj") [{:range (h/->range usage-1-start-pos usage-1-end-pos) :new-text "your-alias/bar"}
                                     {:range (h/->range usage-2-start-pos usage-2-end-pos) :new-text "your-alias/bar"}
                                     {:range (h/->range alias-start-pos alias-end-pos) :new-text "your-alias"}]}
      result)))

(deftest rename-alias-from-alias-definition
  (h/reset-components!)
  (h/load-code-and-locs (h/code "(ns some.ns)"
                                "(defn |foo| [a] 1)"))
  (h/load-code-and-locs
    (h/code "(ns another.ns (:require [some.ns :as other-alias]))"
            "other-alias/foo") (h/file-uri "file:///b.clj"))
  (let [[alias-start-pos alias-end-pos
         usage-1-start-pos usage-1-end-pos
         usage-2-start-pos usage-2-end-pos] (h/load-code-and-locs
                                              (h/code "(ns other.ns (:require [some.ns :as |my-alias|]))"
                                                      "|my-alias/foo|"
                                                      "(|my-alias/foo| 1)") (h/file-uri "file:///c.clj"))
        [alias-start-r alias-start-c] alias-start-pos
        result (:changes (f.rename/rename-from-position (h/file-uri "file:///c.clj") "your-alias" alias-start-r alias-start-c (h/db)))]
    (h/assert-submap
      {(h/file-uri "file:///c.clj") [{:range (h/->range alias-start-pos alias-end-pos) :new-text "your-alias"}
                                     {:range (h/->range usage-1-start-pos usage-1-end-pos) :new-text "your-alias/foo"}
                                     {:range (h/->range usage-2-start-pos usage-2-end-pos) :new-text "your-alias/foo"}]}
      result)))

(deftest rename-destructuring-or
  (h/reset-components!)
  (let [[key-start-pos key-end-pos
         or-start-pos or-end-pos
         usage-start-pos usage-end-pos] (h/load-code-and-locs (h/code "(let [{:keys [|foo|] :or {|foo| 42}} {:foo 0}]"
                                                                      "  (inc |foo|))"))
        [usage-start-r usage-end-c] usage-start-pos
        result (:changes (f.rename/rename-from-position h/default-uri "bar" usage-start-r usage-end-c (h/db)))]
    (h/assert-submap
      {h/default-uri [{:range (h/->range key-start-pos key-end-pos) :new-text "bar"}
                      {:range (h/->range or-start-pos or-end-pos) :new-text "bar"}
                      {:range (h/->range usage-start-pos usage-end-pos) :new-text "bar"}]}
      result)))

(deftest prepare-rename
  (testing "rename local var"
    (h/reset-components!)
    (let [[[row col]] (h/load-code-and-locs "(let [|a 1] a)")
          result (f.rename/prepare-rename h/default-uri row col (h/db))]
      (is (= {:start {:line 0, :character 6}, :end {:line 0, :character 7}}
             result))))
  (testing "should not rename when on unnamed element"
    (h/reset-components!)
    (let [[[row col]] (h/load-code-and-locs "|[]")
          result (f.rename/prepare-rename h/default-uri row col (h/db))]
      (is (= {:error {:code :invalid-params
                      :message "Can't rename - no element found."}}
             result))))
  (testing "should not rename when element definition is not found"
    (h/reset-components!)
    (let [[[row col]] (h/load-code-and-locs "(defprotocol Foo (bar [|unrenable]))")
          result (f.rename/prepare-rename h/default-uri row col (h/db))]
      (is (= {:error {:code :invalid-params
                      :message "Can't rename - no definition found."}}
             result))))
  (testing "should not rename plain keywords"
    (h/reset-components!)
    (let [[[row col]] (h/load-code-and-locs "|:a")
          result (f.rename/prepare-rename h/default-uri row col (h/db))]
      (is (= {:error {:code :invalid-params
                      :message "Can't rename - only namespaced keywords can be renamed."}}
             result))))
  (testing "when client has valid source-paths but no document-changes capability"
    (h/reset-components!)
    (swap! (h/db*) shared/deep-merge
           {:project-root-uri (h/file-uri "file:///")
            :settings {:source-paths #{(h/file-path "/")}}
            :client-capabilities {:workspace {:workspace-edit {:document-changes false}}}})
    (let [[[row col]] (h/load-code-and-locs "(ns |foo.bar-baz)")]
      (h/assert-submap
        {:error {:code :invalid-params
                 :message "Can't rename - client does not support file renames."}}
        (f.rename/prepare-rename h/default-uri row col (h/db)))))
  (testing "when namespace is defined in multiple files"
    (h/reset-components!)
    (swap! (h/db*) shared/deep-merge
           {:project-root-uri (h/file-uri "file:///")
            :settings {:source-paths #{(h/file-path "/src") (h/file-path "/test")}}
            :client-capabilities {:workspace {:workspace-edit {:document-changes true}}}})
    (h/load-code-and-locs "(ns foo.bar-baz)" (h/file-uri "file:///src/foo/bar_baz.clj"))
    (h/load-code-and-locs "(ns foo.bar)" (h/file-uri "file:///src/foo/bar.clj"))
    (let [test-uri (h/file-uri "file:///test/foo/bar_baz.clj")
          [ns-start
           alias-start alias-end] (h/load-code-and-locs "(ns |foo.bar-baz (:require [foo.bar :as |b|]))" test-uri)
          [ns-row ns-col] ns-start
          [alias-row alias-col] alias-start]
      (h/assert-submap
        {:error {:code :invalid-params
                 :message "Can't rename - namespace is defined in multiple files."}}
        (f.rename/prepare-rename test-uri ns-row ns-col (h/db)))
      (testing "renaming alias is not affected"
        (h/assert-submap
          (h/->range alias-start alias-end)
          (f.rename/prepare-rename test-uri alias-row alias-col (h/db))))))
  (testing "when client has document-changes capability but no valid source-paths"
    (h/reset-components!)
    (swap! (h/db*) shared/deep-merge
           {:project-root-uri (h/file-uri "file:///")
            :settings {:source-paths #{(h/file-path "/bla")}}
            :client-capabilities {:workspace {:workspace-edit {:document-changes true}}}})
    (let [[[row col]] (h/load-code-and-locs "(ns |foo.bar-baz)")]
      (h/assert-submap
        {:error {:code :invalid-params
                 :message "Can't rename - invalid source-paths. Are project :source-paths configured correctly?"}}
        (f.rename/prepare-rename h/default-uri row col (h/db))))))

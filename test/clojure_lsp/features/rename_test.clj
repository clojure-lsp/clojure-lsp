(ns clojure-lsp.features.rename-test
  (:require
   [clojure-lsp.db :as db]
   [clojure-lsp.feature.rename :as f.rename]
   [clojure-lsp.test-helper :as h]
   [clojure.test :refer [deftest is testing]]
   [medley.core :as medley]))

(h/reset-db-after-test)

(deftest rename-simple-keywords
  (let [[a-start _a-stop
         a-binding-start a-binding-stop
         a-local-usage-start a-local-usage-stop] (h/load-code-and-locs
                                                   "|:a| (let [{:keys [:|a|]} {}] |a|)"
                                                   (h/file-uri "file:///a.cljc"))]
    (testing "should not rename plain keywords"
      (let [[row col] a-start
            result (f.rename/rename (h/file-uri "file:///a.cljc") ":b" row col db/db)]
        (is (= {:error {:code :invalid-params
                        :message "Can't rename, only namespaced keywords can be renamed."}}
               result))))

    (testing "should rename local in destructure but not keywords"
      (let [[row col] a-binding-start
            changes (:changes (f.rename/rename (h/file-uri "file:///a.cljc") ":b" row col db/db))]
        (is (= {(h/file-uri "file:///a.cljc")
                [{:new-text "b" :range (h/->range a-binding-start a-binding-stop)}
                 {:new-text "b" :range (h/->range a-local-usage-start a-local-usage-stop)}]}
               changes))))))

(deftest rename-destructuring-keywords
  (let [[a-start a-stop
         a-binding-start a-binding-stop] (h/load-code-and-locs
                                           "|:a/b| (let [{:keys [|:a/b|]} {}] b)"
                                           (h/file-uri "file:///a.cljc"))
        [b-start b-stop
         b-binding-start b-binding-stop] (h/load-code-and-locs
                                           "|:c/d| (let [{:keys [|c/d|]} {}] d)"
                                           (h/file-uri "file:///b.cljc"))
        [c-start c-stop
         c-binding-start c-binding-stop] (h/load-code-and-locs
                                           "|:e/f| (let [{:e/keys [|f|]} {}] f)"
                                           (h/file-uri "file:///c.cljc"))]
    (testing "should rename local in destructure with ':' and keywords if namespaced"
      (let [[row col] a-start
            changes (:changes (f.rename/rename (h/file-uri "file:///a.cljc") ":a/c" row col db/db))]
        (is (= {(h/file-uri "file:///a.cljc")
                [{:new-text ":a/c" :range (h/->range a-start a-stop)}
                 {:new-text ":a/c" :range (h/->range a-binding-start a-binding-stop)}]}
               changes))))
    (testing "should rename local in destructure without ':' and keywords if namespaced"
      (let [[row col] b-start
            changes (:changes (f.rename/rename (h/file-uri "file:///b.cljc") ":c/e" row col db/db))]
        (is (= {(h/file-uri "file:///b.cljc")
                [{:new-text ":c/e" :range (h/->range b-start b-stop)}
                 {:new-text "c/e" :range (h/->range b-binding-start b-binding-stop)}]}
               changes))))
    (testing "should rename local in destructure with namespace on :keys"
      (let [[row col] c-start
            changes (:changes (f.rename/rename (h/file-uri "file:///c.cljc") ":e/f" row col db/db))]
        (is (= {(h/file-uri "file:///c.cljc")
                [{:new-text ":e/f" :range (h/->range c-start c-stop)}
                 {:new-text "f" :range (h/->range c-binding-start c-binding-stop)}]}
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
          (h/file-uri "file:///a.cljc"))]
    (testing "renaming keywords renames correctly namespaced maps as well"
      (let [[row col] a-b-start
            changes (:changes (f.rename/rename (h/file-uri "file:///a.cljc") ":a/g" row col db/db))]
        (is (= {(h/file-uri "file:///a.cljc") [{:new-text ":a/g" :range (h/->range a-b-start a-b-stop)}
                                               {:new-text ":g" :range (h/->range b-ns-start b-ns-stop)}]}
               changes))))))

(deftest rename-namespaces
  (testing "when client has valid source-paths but no document-changes capability"
    (h/clean-db!)
    (swap! db/db medley/deep-merge
           {:project-root-uri (h/file-uri "file:///my-project")
            :settings {:source-paths #{(h/file-path "/my-project/src") (h/file-path "/my-project/test")}}
            :client-capabilities {:workspace {:workspace-edit {:document-changes false}}}})
    (h/load-code-and-locs "(ns foo.bar-baz)" (h/file-uri "file:///my-project/src/foo/bar_baz.clj"))
    (h/assert-submap
      {:error {:code :invalid-params
               :message "Can't rename namespace, client does not support file renames."}}
      (f.rename/rename (h/file-uri "file:///my-project/src/foo/bar_baz.clj") "foo.baz-qux" 1 5 db/db)))
  (testing "when client has document-changes capability but no valid source-paths"
    (h/clean-db!)
    (swap! db/db medley/deep-merge
           {:project-root-uri (h/file-uri "file:///my-project")
            :settings {:source-paths #{(h/file-path "/my-project/bla")}}
            :client-capabilities {:workspace {:workspace-edit {:document-changes true}}}})
    (h/load-code-and-locs "(ns foo.bar-baz)" (h/file-uri "file:///my-project/src/foo/bar_baz.clj"))
    (h/assert-submap
      {:error {:code :invalid-params
               :message "Can't rename namespace, invalid source-paths. Are project :source-paths configured correctly?"}}
      (f.rename/rename (h/file-uri "file:///my-project/src/foo/bar_baz.clj") "foo.baz-qux" 1 5 db/db)))
  (testing "when source-paths are valid and client capabilities has document-changes"
    (h/clean-db!)
    (swap! db/db medley/deep-merge
           {:project-root-uri (h/file-uri "file:///my-project")
            :settings {:source-paths #{(h/file-path "/my-project/src") (h/file-path "/my-project/test")}}
            :client-capabilities {:workspace {:workspace-edit {:document-changes true}}}})
    (h/load-code-and-locs "(ns foo.bar-baz)" (h/file-uri "file:///my-project/src/foo/bar_baz.clj"))
    (is (= {:document-changes
            [{:text-document {:version 0
                              :uri (h/file-uri "file:///my-project/src/foo/bar_baz.clj")}
              :edits [{:range
                       {:start {:line 0 :character 4}
                        :end {:line 0 :character 15}}
                       :new-text "foo.baz-qux"}]}
             {:kind "rename"
              :old-uri (h/file-uri "file:///my-project/src/foo/bar_baz.clj")
              :new-uri (h/file-uri "file:///my-project/src/foo/baz_qux.clj")}]}
           (f.rename/rename (h/file-uri "file:///my-project/src/foo/bar_baz.clj") "foo.baz-qux" 1 5 db/db)))))

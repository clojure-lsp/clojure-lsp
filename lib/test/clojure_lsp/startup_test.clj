(ns clojure-lsp.startup-test
  (:require
   [babashka.fs :as fs]
   [clojure-lsp.clj-depend :as lsp.depend]
   [clojure-lsp.db :as db]
   [clojure-lsp.dep-graph :as dep-graph]
   [clojure-lsp.feature.diagnostics.built-in :as f.diagnostics.built-in]
   [clojure-lsp.feature.diagnostics.custom :as f.diagnostics.custom]
   [clojure-lsp.kondo :as lsp.kondo]
   [clojure-lsp.shared :as shared]
   [clojure-lsp.startup :as startup]
   [clojure.java.io :as io]
   [clojure.test :refer [are deftest is testing]]))

(deftest cache-safe-findings-test
  (testing "nil findings stay nil"
    (is (nil? (#'startup/cache-safe-findings nil))))
  (testing "finding entries holding non-serializable values are dropped, plain data is kept"
    ;; e.g. metabase hooks reg-finding! with the node meta, which includes
    ;; :clj-kondo/ignore holding a rewrite-clj node whose :seq-fn is clojure.core/vec
    (is (= {"file:///a.clj" [{:type :some-custom-linter :row 1 :message "boom"}
                             {:type :unused-namespace :langs '() :level :warning}]}
           (#'startup/cache-safe-findings
            {"file:///a.clj" [{:type :some-custom-linter :row 1 :message "boom"
                               :clj-kondo/ignore {:linters [{:tag :vector :seq-fn vec}]}}
                              {:type :unused-namespace :langs '() :level :warning}]})))))

(deftest consider-local-db-cache?-test
  (are [session-analysis-type cache-analysis-type result]
       (= result
          (#'startup/consider-local-db-cache?
           {:project-analysis-type session-analysis-type}
           {:project-analysis-type cache-analysis-type}))
    :project-and-full-dependencies :project-and-full-dependencies true
    :project-and-full-dependencies :project-and-shallow-analysis false
    :project-and-full-dependencies :project-only false

    :project-and-shallow-analysis :project-and-full-dependencies true
    :project-and-shallow-analysis :project-and-shallow-analysis true
    :project-and-shallow-analysis :project-only false

    :project-only :project-and-full-dependencies true
    :project-only :project-and-shallow-analysis true
    :project-only :project-only true))

(deftest load-db-cache!-test
  (let [uri "zipfile:///foo.jar::foo/bar.clj"
        analysis {uri {:var-definitions [{:uri uri :name 'foo :bucket :var-definitions}]}}]
    (testing "reuses the cached dep-graph and documents instead of replaying refresh-analysis"
      (let [dep-graph '{foo.bar {:uris #{"zipfile:///foo.jar::foo/bar.clj"} :internal? false}}
            documents {uri {:namespaces #{'foo.bar} :internal? false :langs #{:clj}}}]
        (with-redefs [db/read-local-cache (constantly {:project-analysis-type :project-and-full-dependencies
                                                       :analysis analysis
                                                       :dep-graph dep-graph
                                                       :documents documents})
                      lsp.kondo/canonicalize-java-analysis identity
                      dep-graph/refresh-analysis (fn [& _] (throw (ex-info "should not rebuild dep-graph" {})))]
          (let [db* (atom {:project-analysis-type :project-and-full-dependencies})]
            (#'startup/load-db-cache! "/project" db*)
            (is (= dep-graph (:dep-graph @db*)))
            (is (= documents (:documents @db*)))
            (is (= analysis (:analysis @db*)))))))
    (testing "rebuilds the dep-graph when the cache has none (older cache format)"
      (with-redefs [db/read-local-cache (constantly {:project-analysis-type :project-and-full-dependencies
                                                     :analysis analysis})
                    lsp.kondo/canonicalize-java-analysis identity
                    dep-graph/refresh-analysis (fn [db _old _new _internal?] (assoc db :rebuilt? true))]
        (let [db* (atom {:project-analysis-type :project-and-full-dependencies})]
          (#'startup/load-db-cache! "/project" db*)
          (is (true? (:rebuilt? @db*)))
          (is (= analysis (:analysis @db*))))))))

(deftest db-without-uris-test
  (let [uri "file:///a.clj"
        kept "file:///b.clj"
        db {:analysis {uri {:var-definitions [{:name 'a}]} kept {:var-definitions [{:name 'b}]}}
            :documents {uri {:internal? true} kept {:internal? true}}
            :dep-graph '{a {:uris #{"file:///a.clj"}} b {:uris #{"file:///b.clj"}}}
            :diagnostics {:clj-kondo {uri [{:code :x}] kept [{:code :y}]}}}
        result (lsp.kondo/db-without-uris db [uri])]
    (is (nil? (get-in result [:analysis uri])))
    (is (nil? (get-in result [:documents uri])))
    (is (nil? (get-in result [:diagnostics :clj-kondo uri])))
    (testing "other uris are kept"
      (is (some? (get-in result [:analysis kept])))
      (is (some? (get-in result [:diagnostics :clj-kondo kept]))))))

(deftest analyze-source-paths!-incremental-test
  (let [dir (str (fs/create-temp-dir))
        a-file (io/file dir "a.clj")
        b-file (io/file dir "b.clj")
        _ (spit a-file "(ns a)")
        _ (spit b-file "(ns b)")
        a-path (.getCanonicalPath a-file)
        b-path (.getCanonicalPath b-file)
        c-path (.getCanonicalPath (io/file dir "c.clj")) ;; never created => a deleted file
        analyzed (atom nil)
        evicted (atom nil)]
    (with-redefs [lsp.kondo/run-kondo-on-paths! (fn [paths _ _ _] (reset! analyzed (set paths)) {})
                  lsp.kondo/db-without-uris (fn [db uris] (reset! evicted (set uris)) db)
                  lsp.kondo/db-with-results (fn [db _] db)
                  lsp.depend/analyze-paths! (fn [_ _] {})
                  lsp.depend/db-with-results (fn [db _] db)
                  f.diagnostics.built-in/db-with-results (fn [db _] db)
                  f.diagnostics.custom/db-with-results (fn [db _] db)
                  shared/filename->uri (fn [f _] (str "file://" f))]
      (testing "warm: re-analyzes only changed/added files and evicts deleted"
        (reset! analyzed nil)
        (reset! evicted nil)
        (let [db* (atom {:source-paths-checksums {a-path (.lastModified a-file)
                                                  b-path 0
                                                  c-path 123}})
              changed? (#'startup/analyze-source-paths! [dir] db* (fn [_]) false)]
          (is (= #{b-path} @analyzed))
          (is (= #{(str "file://" c-path)} @evicted))
          (is (true? changed?))
          (is (= {a-path (.lastModified a-file) b-path (.lastModified b-file)}
                 (:source-paths-checksums @db*)))))
      (testing "warm: nothing changed => no analysis, no eviction, returns false"
        (reset! analyzed nil)
        (reset! evicted nil)
        (let [db* (atom {:source-paths-checksums {a-path (.lastModified a-file)
                                                  b-path (.lastModified b-file)}})
              changed? (#'startup/analyze-source-paths! [dir] db* (fn [_]) false)]
          (is (nil? @analyzed))
          (is (= #{} @evicted))
          (is (false? changed?))))
      (testing "force-all (cold): analyzes every current source file"
        (reset! analyzed nil)
        (reset! evicted nil)
        (let [db* (atom {})
              changed? (#'startup/analyze-source-paths! [dir] db* (fn [_]) true)]
          (is (= #{a-path b-path} @analyzed))
          (is (= #{} @evicted))
          (is (true? changed?)))))))

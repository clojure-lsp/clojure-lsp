(ns clojure-lsp.db-test
  (:require
   [clojure-lsp.db :as db]
   [clojure.java.io :as io]
   [clojure.test :refer [deftest is testing]]
   [cognitect.transit :as transit])
  (:import
   [java.io IOException]))

(defn ^:private temp-cache-file []
  (doto (java.io.File/createTempFile "clojure-lsp.db-test" ".transit.json")
    (.deleteOnExit)))

(deftest cache-analysis-round-trip-test
  (let [uri "file:///project/a.clj"
        jar-uri "zipfile:///foo.jar::foo/bar.clj"
        cache {:version db/version
               :project-root "/project"
               :classpath ["/project/src"]
               :analysis {uri {:var-definitions [{:uri uri :name 'foo :bucket :var-definitions}]
                               :var-usages [{:uri uri :name 'bar :bucket :var-usages}
                                            {:uri uri :name 'baz :bucket :var-usages}]}
                          jar-uri {:java-class-definitions [{:uri jar-uri :class "foo.Bar" :bucket :java-class-definitions}]}}
               :source-paths-checksums {"/project/src/a.clj" 123456}
               :kondo-findings {uri [{:uri uri :type :unused-namespace :level :warning}]}
               :dep-graph '{foo.bar {:uris #{"zipfile:///foo.jar::foo/bar.clj"}
                                     :internal? false
                                     :aliases {bar 1}}}
               :documents {uri {:namespaces #{'a} :internal? true :langs #{:clj}}
                           jar-uri {:namespaces #{'foo.bar} :internal? false :langs #{:clj}}}}
        cache-file (temp-cache-file)]
    (#'db/upsert-cache! cache cache-file)
    (testing "elements are written without the redundant :uri"
      (let [raw (with-open [is (io/input-stream cache-file)]
                  (transit/read (transit/reader is :json)))]
        (is (= [{:name 'foo :bucket :var-definitions}]
               (get-in raw [:analysis uri :var-definitions])))
        (is (= [{:class "foo.Bar" :bucket :java-class-definitions}]
               (get-in raw [:analysis jar-uri :java-class-definitions])))
        (testing "dep-graph, documents and source checksums are written untouched"
          (is (= (:dep-graph cache) (:dep-graph raw)))
          (is (= (:documents cache) (:documents raw)))
          (is (= (:source-paths-checksums cache) (:source-paths-checksums raw)))
          (is (= (:kondo-findings cache) (:kondo-findings raw))))))
    (testing "read cache restores elements :uri"
      (let [read-cache (#'db/read-cache cache-file)]
        (is (= cache read-cache))
        (testing "sharing the analysis key String instance"
          (doseq [[uri buckets] (:analysis read-cache)
                  [_bucket elements] buckets
                  element elements]
            (is (identical? uri (:uri element)))))))))

(deftest cache-edge-cases-test
  (let [cache-file (temp-cache-file)]
    (testing "cache without :analysis round-trips"
      (#'db/upsert-cache! {:version db/version :project-root "/project"} cache-file)
      (is (= {:version db/version :project-root "/project"}
             (#'db/read-cache cache-file))))
    (testing "cache with other version is ignored"
      (#'db/upsert-cache! {:version (dec db/version) :analysis {}} cache-file)
      (is (nil? (#'db/read-cache cache-file))))))

(deftest cache-write-failure-test
  (testing "when cache write fails, cache file should be unchanged"
    (let [cache-file (temp-cache-file)]
      (#'db/upsert-cache! {:version db/version :project-root "/original-project"} cache-file)
      (with-redefs [transit/write (fn [_writer _cache]
                                    (throw (IOException. "intentionally throwing exception to simulate transit/write failure")))]
        (#'db/upsert-cache! {:version db/version :project-root "/modified-project"} cache-file)
        (is (= {:version db/version :project-root "/original-project"}
               (#'db/read-cache cache-file))))))
  (testing "error during move of temporary file to cache file"
    (let [cache-file (temp-cache-file)]
      (#'db/upsert-cache! {:version db/version :project-root "/original-project"} cache-file)
      (with-redefs [clojure-lsp.db/move-file-atomically (fn [_ _]
                                                          (throw (IOException. "intentionally throwing exception to simulate file move failure")))]
        (#'db/upsert-cache! {:version db/version :project-root "/modified-project"} cache-file)
        (is (= {:version db/version :project-root "/original-project"}
               (#'db/read-cache cache-file)))))))

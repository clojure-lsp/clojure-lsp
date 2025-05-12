(ns integration.api.dump-test
  (:require
   [cheshire.core :as json]
   [clojure.edn :as edn]
   [clojure.test :refer [deftest is testing]]
   [integration.helper :as h]
   [integration.lsp :as lsp]))

(lsp/clean-after-test)

(deftest dump-test
  (testing "dumping whole project return correct edn"
    (with-open [rdr (lsp/cli! "dump"
                              "--project-root" h/root-project-path)]
      (let [result (edn/read-string (slurp rdr))]
        (is (= [:findings
                :dep-graph
                :clj-kondo-settings
                :classpath
                :diagnostics
                :project-root
                :settings
                :source-paths
                :analysis]
               (keys result)))
        (is (h/assert-submap
              {:project-root h/root-project-path
               :source-paths #{(h/project-path->canon-path "test")
                               (h/project-path->canon-path "src")
                               (h/project-path->canon-path "resources")}}
              (update result :source-paths set))))))
  (testing "dumping as json filtering specific keys"
    (with-open [rdr (lsp/cli! "dump"
                              "--project-root" h/root-project-path
                              "--output" (str {:format :json
                                               :filter-keys [:project-root :source-paths]}))]
      (let [result (json/parse-string (slurp rdr))]
        (is (= {"project-root" h/root-project-path
                "source-paths" #{(h/project-path->canon-path "test")
                                 (h/project-path->canon-path "src")
                                 (h/project-path->canon-path "resources")}}
               (update result "source-paths" set))))))
  (testing "dumping with analysis type :project-and-shallow-analysis"
    (with-open [rdr (lsp/cli! "dump"
                              "--project-root" h/root-project-path
                              "--analysis" (str {:type :project-and-shallow-analysis}))]
      (let [result (edn/read-string (slurp rdr))]
        (is (seq (keys result)))))))

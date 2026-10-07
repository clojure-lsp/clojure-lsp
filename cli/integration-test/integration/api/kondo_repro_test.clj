(ns integration.api.kondo-repro-test
  (:require
   [clojure.edn :as edn]
   [clojure.string :as string]
   [clojure.test :refer [deftest is testing]]
   [integration.helper :as h]
   [integration.lsp :as lsp]))

(lsp/clean-after-test)

(deftest kondo-repro
  (testing "generates a script linting the given file like the editor does"
    (with-open [rdr (lsp/cli! "kondo-repro"
                              "--project-root" h/root-project-path
                              "--filenames" "src/sample_test/api/diagnostics/a.clj")]
      (let [script (slurp rdr)]
        (is (string/starts-with? script "#!/bin/sh"))
        (is (string/includes? script " -M -m clj-kondo.main "))
        (is (string/includes? script "--lint -"))
        (is (string/includes? script (h/project-path->canon-path "src/sample_test/api/diagnostics/a.clj"))))))
  (testing "output format edn"
    (with-open [rdr (lsp/cli! "kondo-repro"
                              "--project-root" h/root-project-path
                              "--output" "{:format :edn}")]
      (let [{:keys [project-root analysis-type steps]} (edn/read-string (slurp rdr))]
        (is (= h/root-project-path project-root))
        (is (= :project-and-full-dependencies analysis-type))
        (is (= [:copy-configs :external-paths :internal-paths]
               (distinct (map :id steps))))))))

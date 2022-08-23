(ns integration.classpath-test
  (:require
   [babashka.fs :as fs]
   [clojure.string :as str]
   [clojure.test :refer [deftest is]]
   [integration.fixture :as fixture]
   [integration.lsp :as lsp]))

(lsp/clean-after-test)

(defn getRootPath [sample-test-dir]
  (-> (fs/canonicalize ".")
      (fs/path "integration-test" sample-test-dir)))

(defn getRootUri
  "Return the URI to the given SAMPLE-TEST-DIR."
  [sample-test-dir]
  (-> (getRootPath sample-test-dir)
      .toUri .toString))

(defn delete-lsp-cache [sample-test-dir]
  (-> (getRootPath sample-test-dir)
      (fs/path ".lsp" ".cache")
      fs/delete-tree))

(defn classpath-test-project [sample-test-dir]
  (delete-lsp-cache sample-test-dir)
  (lsp/start-process!)
  (lsp/request! [:initialize {:rootUri (getRootUri sample-test-dir)
                              :initializationOptions fixture/default-init-options}])
  (let [{:keys [classpath] :as _res} (lsp/request! ["clojure/serverInfo/raw" {}])]
    (is (some #(str/includes? % "datomic-free") classpath))))

(deftest claspath-babashka
  (classpath-test-project "sample-test-bb"))

(deftest claspath-boot
  (classpath-test-project "sample-test-boot"))

(deftest claspath-cli
  (classpath-test-project "sample-test"))

(deftest claspath-lein
  (classpath-test-project "sample-test-lein"))

(deftest claspath-shadow
  (classpath-test-project "sample-test-shadow"))

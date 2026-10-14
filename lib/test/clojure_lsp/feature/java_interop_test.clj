(ns clojure-lsp.feature.java-interop-test
  (:require
   [babashka.fs :as fs]
   [clojure-lsp.config :as config]
   [clojure-lsp.db :as db]
   [clojure-lsp.feature.java-interop :as f.java-interop]
   [clojure-lsp.shared :as shared]
   [clojure-lsp.test-helper.internal :as h]
   [clojure.java.io :as io]
   [clojure.test :refer [deftest is testing]]
   [medley.core :as medley]))

(h/reset-components-before-test)

(deftest load-java-path-does-not-write-global-cache-test
  (with-redefs [db/read-and-update-global-cache! (fn [_]
                                                   (is false "Test fixtures must not write the global JDK cache"))]
    (h/load-java-path (str (fs/canonicalize "test/fixtures/java_interop/Parent.java"))))
  (is (some #(= "my_class.Parent" (:class %))
            (mapcat :java-class-definitions (vals (:analysis (h/db)))))))

(deftest retrieve-jdk-source-with-incomplete-cache-test
  (let [cache-dir (fs/create-temp-dir {:prefix "clojure-lsp-jdk-cache-test"})
        jdk-dir (io/file (str cache-dir) "jdk")
        java-file (io/file jdk-dir "java.base" "java" "net" "URI.java")
        java-path (.getCanonicalPath java-file)
        java-uri (shared/filename->uri java-path (h/db))
        global-db* (atom {:version db/version
                          :analysis {"file:///fixtures/Parent.java"
                                     {:java-class-definitions [{:class "my_class.Parent"}]}}})]
    (try
      (io/make-parents java-file)
      (spit java-file "package java.net; public class URI {}")
      (spit (io/file jdk-dir "result") "file:///jdk/src.zip")
      (with-redefs [config/global-cache-dir (constantly (io/file (str cache-dir)))
                    db/read-global-cache #(deref global-db*)
                    db/read-and-update-global-cache! #(swap! global-db* %)]
        (testing "unrelated cached definitions do not skip JDK analysis"
          (f.java-interop/retrieve-jdk-source-and-analyze! (h/db*))
          (is (= "java.net.URI"
                 (-> (h/db) :analysis (get java-uri) :java-class-definitions first :class)))
          (is (contains? (:analysis-checksums @global-db*) java-path)))
        (testing "the repaired cache is loaded on the next startup"
          (h/reset-components!)
          (with-redefs [f.java-interop/analyze-and-cache-jdk-source! (fn [& _]
                                                                       (is false "A complete cache should be reused"))]
            (f.java-interop/retrieve-jdk-source-and-analyze! (h/db*)))
          (is (= "java.net.URI"
                 (-> (h/db) :analysis (get java-uri) :java-class-definitions first :class)))))
      (finally
        (fs/delete-tree cache-dir)))))

(deftest uri->translated-uri-test
  (testing "common files"
    (is (= "" (f.java-interop/uri->translated-uri "" (h/db) (h/producer))))
    (is (= "/foo/bar.clj" (f.java-interop/uri->translated-uri "/foo/bar.clj" (h/db) (h/producer))))
    (is (= "file:///foo/bar.clj" (f.java-interop/uri->translated-uri "file:///foo/bar.clj" (h/db) (h/producer))))
    (is (= "jar:file:///foo/bar.clj" (f.java-interop/uri->translated-uri "jar:file:///foo/bar.clj" (h/db) (h/producer))))
    (is (= "jar:file:///foo.jar!/bar.clj" (f.java-interop/uri->translated-uri "jar:file:///foo.jar!/bar.clj" (h/db) (h/producer))))
    (is (= "jar:file:///foo.jar!/Bar.java" (f.java-interop/uri->translated-uri "jar:file:///foo.jar!/Bar.java" (h/db) (h/producer))))
    (is (= "zipfile:///foo.jar::/bar.clj" (f.java-interop/uri->translated-uri "zipfile:///foo.jar::/bar.clj" (h/db) (h/producer))))
    (is (= "zipfile:///foo.jar::/bar.java" (f.java-interop/uri->translated-uri "zipfile:///foo.jar::/bar.java" (h/db) (h/producer)))))
  (testing "class files"
    (with-redefs [f.java-interop/decompile-file (fn [_jar entry _db]
                                                  (.getName entry))]
      (let [test-jar-file-url (io/as-url (io/file "test/fixtures/java_interop/single-class.jar"))]
        (testing "jar scheme"
          (is (= "Bar.class" (f.java-interop/uri->translated-uri (str "jar:" test-jar-file-url "!/Bar.class") (h/db) (h/producer)))))
        (testing "zipfile scheme"
          (is (= "Bar.class" (f.java-interop/uri->translated-uri (str "zip" test-jar-file-url "::Bar.class") (h/db) (h/producer)))))))))

(defn ->decision [& args]
  (-> (apply #'f.java-interop/jdk-analysis-decision args)
      (medley/update-existing :jdk-zip-file #(when % (str %)))))

(deftest jdk-analysis-decision-test
  (testing "when jdk is already installed"
    (testing "No custom source URI"
      (is (= {:result :jdk-already-installed}
             (->decision (h/file-uri "file:///path/to/jdk.zip") nil (ref nil) false))))
    (testing "custom source URI is the same as installed one"
      (is (= {:result :jdk-already-installed}
             (->decision (h/file-uri "file:///path/to/jdk.zip") (h/file-uri "file:///path/to/jdk.zip") (ref nil) false))))
    (testing "custom source URI is not the same as installed one"
      (is (= {:result :no-source-found}
             (->decision (h/file-uri "file:///path/to/jdk.zip") "https:///other/jdk.zip" (ref nil) false)))))
  (testing "When jdk is not installed yet"
    (testing "when we find a local JDK automatically"
      (is (= {:result :automatic-local-jdk
              :jdk-zip-file "jdk-file"}
             (->decision nil nil (ref "jdk-file") false))))
    (testing "when we find a local JDK automatically but a custom uri is provided"
      (is (= {:result :manual-local-jdk
              :jdk-zip-file (h/file-path "/path/to/jdk.zip")}
             (->decision nil (h/file-uri "file:///path/to/jdk.zip") (ref :jdk-file) false))))
    (testing "A custom uri is provided as local URI"
      (is (= {:result :manual-local-jdk
              :jdk-zip-file (h/file-path "/path/to/jdk.zip")}
             (->decision nil (h/file-uri "file:///path/to/jdk.zip") (ref nil) false))))
    (testing "A custom uri is provided as local path"
      (is (= {:result :manual-local-jdk
              :jdk-zip-file (h/file-path "/path/to/jdk.zip")}
             (->decision nil (h/file-path "/path/to/jdk.zip") (ref nil) false))))
    (testing "A custom uri is provided as unknown uri"
      (is (= {:result :manual-local-jdk
              :jdk-zip-file (h/file-path "foo:/asd")}
             (->decision nil (h/file-uri "foo:///asd") (ref nil) false))))
    (testing "A custom uri is provided as external URI but download setting is false"
      (is (= {:result :no-source-found}
             (->decision nil "https://path/to/my/jdk.zip" (ref nil) false))))
    (testing "A custom uri is provided as external URI and download setting is true"
      (is (= {:result :download-jdk
              :download-uri "https://path/to/my/jdk.zip"}
             (->decision nil "https://path/to/my/jdk.zip" (ref nil) true))))
    (testing "No custom uri, JDK not found and download setting is true"
      (is (= {:result :download-jdk
              :download-uri "https://raw.githubusercontent.com/clojure-lsp/jdk-source/main/openjdk-19/reduced/source.zip"}
             (->decision nil nil (ref nil) true))))))

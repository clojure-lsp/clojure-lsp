(ns clojure-lsp.feature.java-interop-test
  (:require
   [clojure-lsp.feature.java-interop :as f.java-interop]
   [clojure-lsp.test-helper :as h]
   [clojure.test :refer [deftest is testing]]
   [medley.core :as medley]))

(h/reset-components-before-test)

(deftest uri->translated-uri-test
  (is (= "" (f.java-interop/uri->translated-uri "" (h/db) (h/producer))))
  (is (= "/foo/bar.clj" (f.java-interop/uri->translated-uri "/foo/bar.clj" (h/db) (h/producer))))
  (is (= "file:///foo/bar.clj" (f.java-interop/uri->translated-uri "file:///foo/bar.clj" (h/db) (h/producer))))
  (is (= "jar:file:///foo/bar.clj" (f.java-interop/uri->translated-uri "jar:file:///foo/bar.clj" (h/db) (h/producer))))
  (is (= "jar:file:///foo.jar!/bar.clj" (f.java-interop/uri->translated-uri "jar:file:///foo.jar!/bar.clj" (h/db) (h/producer))))
  (is (= "jar:file:///foo.jar!/Bar.java" (f.java-interop/uri->translated-uri "jar:file:///foo.jar!/Bar.java" (h/db) (h/producer))))
  (is (= "zipfile:///foo.jar::/bar.clj" (f.java-interop/uri->translated-uri "zipfile:///foo.jar::/bar.clj" (h/db) (h/producer))))
  (is (= "zipfile:///foo.jar::/bar.java" (f.java-interop/uri->translated-uri "zipfile:///foo.jar::/bar.java" (h/db) (h/producer)))))

(defn ->decision [& args]
  (-> (apply #'f.java-interop/jdk-analysis-decision args)
      (medley/update-existing :jdk-zip-file #(when % (str %)))))

(deftest jdk-analysis-decision-test
  (testing "when jdk is already installed"
    (testing "No custom source URI"
      (is (= {:result :jdk-already-installed}
             (->decision "file:///path/to/jdk.zip" nil (ref nil) false))))
    (testing "custom source URI is the same as installed one"
      (is (= {:result :jdk-already-installed}
             (->decision "file:///path/to/jdk.zip" "file:///path/to/jdk.zip" (ref nil) false))))
    (testing "custom source URI is not the same as installed one"
      (is (= {:result :no-source-found}
             (->decision "file:///path/to/jdk.zip" "https:///other/jdk.zip" (ref nil) false)))))
  (testing "When jdk is not installed yet"
    (testing "when we find a local JDK automatically"
      (is (= {:result :automatic-local-jdk
              :jdk-zip-file "jdk-file"}
             (->decision nil nil (ref "jdk-file") false))))
    (testing "when we find a local JDK automatically but a custom uri is provided"
      (is (= {:result :manual-local-jdk
              :jdk-zip-file "/path/to/jdk.zip"}
             (->decision nil "file:///path/to/jdk.zip" (ref :jdk-file) false))))
    (testing "A custom uri is provided as local URI"
      (is (= {:result :manual-local-jdk
              :jdk-zip-file "/path/to/jdk.zip"}
             (->decision nil "file:///path/to/jdk.zip" (ref nil) false))))
    (testing "A custom uri is provided as local path"
      (is (= {:result :manual-local-jdk
              :jdk-zip-file "/path/to/jdk.zip"}
             (->decision nil "/path/to/jdk.zip" (ref nil) false))))
    (testing "A custom uri is provided as unknown uri"
      (is (= {:result :manual-local-jdk
              :jdk-zip-file "foo:/asd"}
             (->decision nil "foo:///asd" (ref nil) false))))
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

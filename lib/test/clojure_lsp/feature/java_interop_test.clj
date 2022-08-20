(ns clojure-lsp.feature.java-interop-test
  (:require
   [clojure-lsp.feature.java-interop :as f.java-interop]
   [clojure-lsp.test-helper :as h]
   [clojure.test :refer [deftest is testing]]
   [medley.core :as medley]))

(deftest uri->translated-uri-test
  (let [{:keys [db producer]} (h/make-components)]
    (is (= "" (f.java-interop/uri->translated-uri "" db producer)))
    (is (= "/foo/bar.clj" (f.java-interop/uri->translated-uri "/foo/bar.clj" db producer)))
    (is (= "file:///foo/bar.clj" (f.java-interop/uri->translated-uri "file:///foo/bar.clj" db producer)))
    (is (= "jar:file:///foo/bar.clj" (f.java-interop/uri->translated-uri "jar:file:///foo/bar.clj" db producer)))
    (is (= "jar:file:///foo.jar!/bar.clj" (f.java-interop/uri->translated-uri "jar:file:///foo.jar!/bar.clj" db producer)))
    (is (= "jar:file:///foo.jar!/Bar.java" (f.java-interop/uri->translated-uri "jar:file:///foo.jar!/Bar.java" db producer)))
    (is (= "zipfile:///foo.jar::/bar.clj" (f.java-interop/uri->translated-uri "zipfile:///foo.jar::/bar.clj" db producer)))
    (is (= "zipfile:///foo.jar::/bar.java" (f.java-interop/uri->translated-uri "zipfile:///foo.jar::/bar.java" db producer)))))

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

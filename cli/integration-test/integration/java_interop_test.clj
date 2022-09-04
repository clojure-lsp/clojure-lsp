(ns integration.java-interop-test
  (:require
   [clojure.java.io :as io]
   [clojure.string :as string]
   [clojure.test :refer [deftest is testing]]
   [integration.fixture :as fixture]
   [integration.helper :as h]
   [integration.lsp :as lsp]))

(lsp/clean-after-test)

(deftest find-definition-of-java-class-when-source-exists
  (lsp/start-process!)
  (lsp/request! (fixture/initialize-request
                  {:initializationOptions (dissoc fixture/default-init-options :java)}))
  (lsp/notify! (fixture/initialized-notification))
  (lsp/notify! (fixture/did-open-notification "java_interop/a.clj"))

  (testing "We find java source class"
    (h/assert-submap
      {:uri (h/source-path->uri "java_interop/SampleClass.java")
       :range {:start {:line 0 :character 0}
               :end {:line 0 :character 0}}}
      (lsp/request! (fixture/definition-request "java_interop/a.clj" 7 5)))))

(deftest find-definition-of-java-class-when-source-does-not-exists
  (h/delete-project-file "../../.lsp/.cache/java")
  (lsp/start-process!)
  (lsp/request! (fixture/initialize-request
                  {:initializationOptions (-> fixture/default-init-options
                                              (assoc :dependency-scheme "jar")
                                              (dissoc :java)
                                              (assoc-in [:java :decompile-jar-as-project?] false))})) ;; TODO fix when add support for decompile with zipfile
  (lsp/notify! (fixture/initialized-notification))
  (lsp/notify! (fixture/did-open-notification "java_interop/a.clj"))

  (let [result (lsp/request! (fixture/definition-request "java_interop/a.clj" 8 5))]
    (testing "We find java compiled class first"
      (h/assert-submap
        {:uri (-> "integration-test/sample-test/.lsp/.cache/java/decompiled/clojure/lang/PersistentVector.java"
                  io/as-relative-path
                  io/file
                  h/file->uri)
         :range {:start {:line 0 :character 0}
                 :end {:line 0 :character 0}}}
        result))

    (testing "we decompile the class file and get its contents"
      (let [class-content (lsp/request! (fixture/clojure-dependency-contents-request (:uri result)))]
        (is (string/includes? class-content "Decompiled with CFR"))
        (is (string/includes? class-content "class PersistentVector")))))

  (h/delete-project-file "../../.lsp/.cache/java"))

(ns integration.java-interop-test
  (:require
   [clojure.string :as string]
   [clojure.test :refer [deftest is testing]]
   [integration.fixture :as fixture]
   [integration.helper :as h]
   [integration.lsp :as lsp]))

(lsp/clean-after-test)

#_(deftest find-definition-of-java-class-when-source-exists
    (lsp/start-process!)
    (lsp/request! (fixture/initialize-request
                    {:initializationOptions fixture/default-init-options}))
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
                  {:initializationOptions fixture/default-init-options}))
  (lsp/notify! (fixture/initialized-notification))
  (lsp/notify! (fixture/did-open-notification "java_interop/a.clj"))

  (let [result (lsp/request! (fixture/definition-request "java_interop/a.clj" 8 5))]
    (testing "We find java compiled class first"
      (h/assert-submap
        {:range {:start {:line 0 :character 0}
                 :end {:line 0 :character 0}}}
        result)
      (is (string/includes? (:uri result) ".jar::clojure/lang/PersistentVector.class")))

    (testing "we decompile the class file and get its contents"
      (let [class-content (lsp/request! (fixture/clojure-dependency-contents-request (:uri result)))]
        (is (string/includes? class-content "Decompiled with CFR"))
        (is (string/includes? class-content "class PersistentVector")))))

  (h/delete-project-file "../../.lsp/.cache/java"))

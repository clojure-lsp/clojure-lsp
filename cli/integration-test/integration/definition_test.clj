(ns integration.definition-test
  (:require
   [clojure.string :as string]
   [clojure.test :refer [deftest testing]]
   [integration.fixture :as fixture]
   [integration.helper :as h]
   [integration.lsp :as lsp]))

(lsp/clean-after-test)

(deftest definition
  (lsp/start-process!)
  (lsp/request! (fixture/initialize-request))
  (lsp/notify! (fixture/initialized-notification))
  (lsp/notify! (fixture/did-open-source-path-notification "definition/a.clj"))
  (lsp/notify! (fixture/did-open-source-path-notification "definition/b.clj"))

  (testing "common vars"
    (testing "find definition on same ns"
      (h/assert-submap
        {:uri (h/source-path->uri "definition/a.clj")
         :range {:start {:line 8 :character 16}
                 :end {:line 8 :character 33}}}
        (lsp/request! (fixture/definition-request (h/source-path->uri "definition/a.clj") 11 2))))

    (testing "find definition of local var"
      (h/assert-submap
        {:uri (h/source-path->uri "definition/a.clj")
         :range {:start {:line 3 :character 5}
                 :end {:line 3 :character 13}}}
        (lsp/request! (fixture/definition-request (h/source-path->uri "definition/a.clj") 9 9))))

    (testing "find definition of public var"
      (h/assert-submap
        {:uri (h/source-path->uri "definition/a.clj")
         :range {:start {:line 5 :character 6}
                 :end {:line 5 :character 22}}}
        (lsp/request! (fixture/definition-request (h/source-path->uri "definition/b.clj") 3 4)))))

  (testing "keywords"
    (testing "definition of local simple keyword is the keyword itself"
      (h/assert-submap
        {:uri (h/source-path->uri "definition/b.clj")
         :range {:start {:line 5 :character 0}
                 :end {:line 5 :character 17}}}
        (lsp/request! (fixture/definition-request (h/source-path->uri "definition/b.clj") 5 2))))

    (testing "definition of invalid local namespaced keyword is the keyword itself"
      (h/assert-submap
        {:uri (h/source-path->uri "definition/b.clj")
         :range {:start {:line 7 :character 0}
                 :end {:line 7 :character 8}}}
        (lsp/request! (fixture/definition-request (h/source-path->uri "definition/b.clj") 7 2))))

    (testing "find definition of valid other ns namespaced keyword"
      (h/assert-submap
        {:uri (h/source-path->uri "definition/a.clj")
         :range {:start {:line 13 :character 7}
                 :end {:line 13 :character 15}}}
        (lsp/request! (fixture/definition-request (h/source-path->uri "definition/b.clj") 9 2))))

    (testing "find definition of valid other aliased keyword"
      (h/assert-submap
        {:uri (h/source-path->uri "definition/a.clj")
         :range {:start {:line 13 :character 7}
                 :end {:line 13 :character 15}}}
        (lsp/request! (fixture/definition-request (h/source-path->uri "definition/b.clj") 11 2))))))

(deftest definition-external-dependency-jar-scheme
  (h/delete-project-file "../../.lsp/.cache/")
  (lsp/start-process!)
  (lsp/request! (fixture/initialize-request {:initializationOptions (-> fixture/default-init-options
                                                                        (assoc :dependency-scheme "jar"))}))
  (lsp/notify! (fixture/initialized-notification))
  (lsp/notify! (fixture/did-open-source-path-notification "definition/a.clj"))

  (let [{:keys [uri]} (lsp/request! (fixture/definition-request (h/source-path->uri "definition/a.clj") 15 2))]
    (lsp/notify! (fixture/did-open-external-path-notification uri (slurp uri)))

    (testing "LSP features work on external clojure opened files"
      (h/assert-submap
        {:language "clojure"
         :value "[x]\n[x message]"}
        (-> (lsp/request! (fixture/hover-external-uri-request uri 7612 5))
            :contents
            (get 1))))))

(deftest definition-external-dependency-zipfile-scheme
  (h/delete-project-file "../../.lsp/.cache/")
  (lsp/start-process!)
  (lsp/request! (fixture/initialize-request))
  (lsp/notify! (fixture/initialized-notification))
  (lsp/notify! (fixture/did-open-source-path-notification "definition/a.clj"))

  (let [{:keys [uri]} (lsp/request! (fixture/definition-request (h/source-path->uri "definition/a.clj") 15 2))]
    (lsp/notify! (fixture/did-open-external-path-notification uri (-> uri
                                                                      (string/replace "zipfile:" "jar:file:")
                                                                      (string/replace "::" "!/")
                                                                      slurp)))

    (testing "LSP features work on external clojure opened files"
      (h/assert-submap
        {:language "clojure"
         :value "[x]\n[x message]"}
        (-> (lsp/request! (fixture/hover-external-uri-request uri 7612 5))
            :contents
            (get 1))))))

(deftest definition-external-dependency-zipfile-scheme-escaped-uris
  (h/delete-project-file "../../.lsp/.cache/")
  (with-redefs [h/*escape-uris?* true]
    (lsp/start-process!)
    (lsp/request! (fixture/initialize-request))
    (lsp/notify! (fixture/initialized-notification))
    (lsp/notify! (fixture/did-open-source-path-notification "definition/a.clj"))

    (let [{:keys [uri]} (lsp/request! (fixture/definition-request (h/source-path->uri "definition/a.clj") 15 2))]
      (lsp/notify! (fixture/did-open-external-path-notification (h/escape-uri uri)
                                                                (-> (h/unescape-uri uri)
                                                                    (string/replace "zipfile:" "jar:file:")
                                                                    (string/replace "::" "!/")
                                                                    slurp)))

      (testing "LSP hover feature works on external clojure opened files"
        (h/assert-submap
          {:language "clojure"
           :value "[x]\n[x message]"}
          (-> (lsp/request! (fixture/hover-external-uri-request (h/escape-uri uri) 7612 5))
              :contents
              (get 1))))

      (testing "LSP definition from external clojure opened file works"
        (h/assert-submap
          {:uri uri
           :range {:start {:line 4840 :character 10}
                   :end {:line 4840 :character 16}}}
          (lsp/request! (fixture/definition-request (h/escape-uri uri) 7612 5)))))))

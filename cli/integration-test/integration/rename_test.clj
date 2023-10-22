(ns integration.rename-test
  (:require
   [clojure.test :refer [deftest testing]]
   [integration.fixture :as fixture]
   [integration.helper :as h]
   [integration.lsp :as lsp]))

(lsp/clean-after-test)

(deftest rename-function-definition
  (lsp/start-process!)
  (lsp/request! (fixture/initialize-request))
  (lsp/notify! (fixture/initialized-notification))
  (lsp/notify! (fixture/did-open-source-path-notification "rename/a.cljc"))
  (lsp/notify! (fixture/did-open-source-path-notification "rename/b.cljc"))
  (lsp/notify! (fixture/did-open-source-path-notification "rename/single_a.clj"))
  (lsp/notify! (fixture/did-open-source-path-notification "rename/single_b.clj"))

  (testing "Renaming from the function defintion"
    (h/assert-submap
      {:changes
       {(keyword (h/source-path->uri "rename/b.cljc"))
        [{:range {:start {:line 1 :character 48} :end {:line 1 :character 55}}
          :newText "your-func"}
         {:range {:start {:line 3 :character 3} :end {:line 3 :character 10}}
          :newText "your-func"}
         {:range {:start {:line 5 :character 1} :end {:line 5 :character 8}}
          :newText "your-func"}]
        (keyword (h/source-path->uri "rename/a.cljc"))
        [{:range {:start {:line 3 :character 6} :end {:line 3 :character 13}}
          :newText "your-func"}
         {:range {:start {:line 6 :character 1} :end {:line 6 :character 8}}
          :newText "your-func"}
         {:range {:start {:line 8 :character 1} :end {:line 8 :character 8}}
          :newText "your-func"}]}}
      (lsp/request! (fixture/rename-request "rename/a.cljc" "your-func" 3 9)))))

(deftest rename-function-usage
  (lsp/start-process!)
  (lsp/request! (fixture/initialize-request))
  (lsp/notify! (fixture/initialized-notification))
  (lsp/notify! (fixture/did-open-source-path-notification "rename/a.cljc"))
  (lsp/notify! (fixture/did-open-source-path-notification "rename/b.cljc"))
  (lsp/notify! (fixture/did-open-source-path-notification "rename/single_a.clj"))
  (lsp/notify! (fixture/did-open-source-path-notification "rename/single_b.clj"))

  (testing "Renaming from the function usage on other ns"
    (h/assert-submap
      {:changes
       {(keyword (h/source-path->uri "rename/b.cljc"))
        [{:range {:start {:line 1 :character 48} :end {:line 1 :character 55}}
          :newText "your-func"}
         {:range {:start {:line 3 :character 3} :end {:line 3 :character 10}}
          :newText "your-func"}
         {:range {:start {:line 5 :character 1} :end {:line 5 :character 8}}
          :newText "your-func"}]
        (keyword (h/source-path->uri "rename/a.cljc"))
        [{:range {:start {:line 3 :character 6} :end {:line 3 :character 13}}
          :newText "your-func"}
         {:range {:start {:line 6 :character 1} :end {:line 6 :character 8}}
          :newText "your-func"}
         {:range {:start {:line 8 :character 1} :end {:line 8 :character 8}}
          :newText "your-func"}]}}
      (lsp/request! (fixture/rename-request "rename/b.cljc" "your-func" 3 4)))))

(deftest rename-local-keywords
  (lsp/start-process!)
  (lsp/request! (fixture/initialize-request))
  (lsp/notify! (fixture/initialized-notification))
  (lsp/notify! (fixture/did-open-source-path-notification "rename/a.cljc"))
  (lsp/notify! (fixture/did-open-source-path-notification "rename/b.cljc"))
  (lsp/notify! (fixture/did-open-source-path-notification "rename/single_a.clj"))
  (lsp/notify! (fixture/did-open-source-path-notification "rename/single_b.clj"))

  (testing "Renaming local keywords"
    (h/assert-submap
      {:changes
       {(keyword (h/source-path->uri "rename/a.cljc"))
        [{:range {:start {:line 12 :character 15} :end {:line 12 :character 21}}
          :newText "your-key"}
         {:range {:start {:line 13 :character 7} :end {:line 13 :character 13}}
          :newText "your-key"}]}}
      (with-redefs [h/*escape-uris?* true]
        (lsp/request! (fixture/rename-request "rename/a.cljc" ":your-key" 12 15))))))

(deftest rename-namespaced-keywords
  (lsp/start-process!)
  (lsp/request! (fixture/initialize-request))
  (lsp/notify! (fixture/initialized-notification))
  (lsp/notify! (fixture/did-open-source-path-notification "rename/a.cljc"))
  (lsp/notify! (fixture/did-open-source-path-notification "rename/b.cljc"))
  (lsp/notify! (fixture/did-open-source-path-notification "rename/single_a.clj"))
  (lsp/notify! (fixture/did-open-source-path-notification "rename/single_b.clj"))

  (testing "Renaming namespaced keywords"
    (h/assert-submap
      {:changes
       {(keyword (h/source-path->uri "rename/a.cljc"))
        [{:range {:start {:line 15 :character 0} :end {:line 15 :character 7}}
          :newText "::click-now"}
         {:range {:start {:line 17 :character 0} :end {:line 17 :character 27}}
          :newText ":sample-test.rename.a/click-now"}]

        (keyword (h/source-path->uri "rename/b.cljc"))
        [{:range {:start {:line 7 :character 0} :end {:line 7 :character 9}}
          :newText "::a/click-now"}
         {:range {:start {:line 9 :character 0} :end {:line 9 :character 27}}
          :newText ":sample-test.rename.a/click-now"}]}}
      (lsp/request! (fixture/rename-request "rename/a.cljc" "::click-now" 15 0)))))

(deftest rename-single-namespaced-keywords
  (lsp/start-process!)
  (lsp/request! (fixture/initialize-request))
  (lsp/notify! (fixture/initialized-notification))
  (lsp/notify! (fixture/did-open-source-path-notification "rename/a.cljc"))
  (lsp/notify! (fixture/did-open-source-path-notification "rename/b.cljc"))
  (lsp/notify! (fixture/did-open-source-path-notification "rename/single_a.clj"))
  (lsp/notify! (fixture/did-open-source-path-notification "rename/single_b.clj"))

  (testing "Renaming single-name-namespace'd keywords"
    (h/assert-submap
      {:changes
       {(keyword (h/source-path->uri "rename/single_a.clj"))
        [{:range {:start {:line 2 :character 0} :end {:line 2 :character 7}}
          :newText "::click-now"}
         {:range {:start {:line 4 :character 0} :end {:line 4 :character 34}}
          :newText ":sample-test.rename.single-a/click-now"}]

        (keyword (h/source-path->uri "rename/single_b.clj"))
        [{:range {:start {:line 3 :character 0} :end {:line 3 :character 9}}
          :newText "::a/click-now"}
         {:range {:start {:line 5 :character 0} :end {:line 5 :character 34}}
          :newText ":sample-test.rename.single-a/click-now"}]}}
      (lsp/request! (fixture/rename-request "rename/single_a.clj" "::click-now" 2 0)))))

(deftest rename-require-alias
  (lsp/start-process!)
  (lsp/request! (fixture/initialize-request))
  (lsp/notify! (fixture/initialized-notification))
  (lsp/notify! (fixture/did-open-source-path-notification "rename/a.cljc"))
  (lsp/notify! (fixture/did-open-source-path-notification "rename/b.cljc"))
  (lsp/notify! (fixture/did-open-source-path-notification "rename/single_a.clj"))
  (lsp/notify! (fixture/did-open-source-path-notification "rename/single_b.clj"))

  (testing "Renaming require alias"
    (h/assert-submap
      {:changes
       {(keyword (h/source-path->uri "rename/a.cljc"))
        [{:range {:start {:line 1 :character 36} :end {:line 1 :character 37}}
          :newText "spec"}
         {:range {:start {:line 10 :character 1} :end {:line 10 :character 6}}
          :newText "spec/def"}]}}
      (lsp/request! (fixture/rename-request "rename/a.cljc" "spec" 1 36)))))

(deftest rename-require-refer
  (lsp/start-process!)
  (lsp/request! (fixture/initialize-request))
  (lsp/notify! (fixture/initialized-notification))
  (lsp/notify! (fixture/did-open-source-path-notification "rename/a.cljc"))
  (lsp/notify! (fixture/did-open-source-path-notification "rename/b.cljc"))
  (lsp/notify! (fixture/did-open-source-path-notification "rename/single_a.clj"))
  (lsp/notify! (fixture/did-open-source-path-notification "rename/single_b.clj"))

  (testing "Renaming require refer"
    (h/assert-submap
      {:changes
       {(keyword (h/source-path->uri "rename/b.cljc"))
        [{:range {:start {:line 1 :character 48} :end {:line 1 :character 55}}
          :newText "your-func"}
         {:range {:start {:line 3 :character 3} :end {:line 3 :character 10}}
          :newText "your-func"}
         {:range {:start {:line 5 :character 1} :end {:line 5 :character 8}}
          :newText "your-func"}]
        (keyword (h/source-path->uri "rename/a.cljc"))
        [{:range {:start {:line 3 :character 6} :end {:line 3 :character 13}}
          :newText "your-func"}
         {:range {:start {:line 6 :character 1} :end {:line 6 :character 8}}
          :newText "your-func"}
         {:range {:start {:line 8 :character 1} :end {:line 8 :character 8}}
          :newText "your-func"}]}}
      (lsp/request! (fixture/rename-request "rename/b.cljc" "your-func" 1 52)))))

(deftest prepare-rename
  (lsp/start-process!)
  (lsp/request! (fixture/initialize-request))
  (lsp/notify! (fixture/initialized-notification))
  (lsp/notify! (fixture/did-open-source-path-notification "rename/a.cljc"))
  (lsp/notify! (fixture/did-open-source-path-notification "rename/b.cljc"))
  (lsp/notify! (fixture/did-open-source-path-notification "rename/single_a.clj"))
  (lsp/notify! (fixture/did-open-source-path-notification "rename/single_b.clj"))

  (h/assert-submap
    {:error {:code -32602, :message "Can't rename - no element found."}}
    (lsp/request! (fixture/prepare-rename-request "rename/a.cljc" 12 6)))
  (h/assert-submap
    {:start {:line 12, :character 14}, :end {:line 12, :character 21}}
    (lsp/request! (fixture/prepare-rename-request "rename/a.cljc" 12 15))))

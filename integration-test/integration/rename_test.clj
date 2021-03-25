(ns integration.rename-test
  (:require
    [clojure.test :refer [deftest testing]]
    [integration.fixture :as fixture]
    [integration.lsp :as lsp]
    [integration.helper :as h]))

(h/clean-after-test)

(deftest rename
  (lsp/start-process!)
  (lsp/request! (fixture/initialize-request))
  (lsp/notify! (fixture/initialized-notification))
  (lsp/notify! (fixture/did-open-notification "rename/a.clj"))
  (lsp/notify! (fixture/did-open-notification "rename/b.clj"))

  (testing "Renaming from the function defintion"
    (h/assert-submap
      {:changes
       {(keyword (h/source-path->uri "rename/b.clj"))
        [{:range {:start {:line 5 :character 1} :end {:line 5 :character 8}}
          :newText "your-func"}
         {:range {:start {:line 3 :character 3} :end {:line 3 :character 10}}
          :newText "your-func"}
         {:range {:start {:line 1 :character 36} :end {:line 1 :character 43}}
          :newText "your-func"}]
        (keyword (h/source-path->uri "rename/a.clj"))
        [{:range {:start {:line 3 :character 6} :end {:line 3 :character 13}}
          :newText "your-func"}
         {:range {:start {:line 6 :character 1} :end {:line 6 :character 8}}
          :newText "your-func"}
         {:range {:start {:line 8 :character 1} :end {:line 8 :character 8}}
          :newText "your-func"}]}}
      (lsp/request! (fixture/rename-request "rename/a.clj" "your-func" 3 9))))

  (testing "Renaming from the function usage on other ns"
    (h/assert-submap
      {:changes
       {(keyword (h/source-path->uri "rename/b.clj"))
        [{:range {:start {:line 5 :character 1} :end {:line 5 :character 8}}
          :newText "your-func"}
         {:range {:start {:line 3 :character 3} :end {:line 3 :character 10}}
          :newText "your-func"}
         {:range {:start {:line 1 :character 36} :end {:line 1 :character 43}}
          :newText "your-func"}]
        (keyword (h/source-path->uri "rename/a.clj"))
        [{:range {:start {:line 3 :character 6} :end {:line 3 :character 13}}
          :newText "your-func"}
         {:range {:start {:line 6 :character 1} :end {:line 6 :character 8}}
          :newText "your-func"}
         {:range {:start {:line 8 :character 1} :end {:line 8 :character 8}}
          :newText "your-func"}]}}
      (lsp/request! (fixture/rename-request "rename/b.clj" "your-func" 3 4))))

  (testing "Renaming local keywords"
    (h/assert-submap
      {:changes
       {(keyword (h/source-path->uri "rename/a.clj"))
        [{:range {:start {:line 12 :character 15} :end {:line 12 :character 21}}
          :newText "your-key"}
         {:range {:start {:line 13 :character 7} :end {:line 13 :character 13}}
          :newText "your-key"}]}}
      (lsp/request! (fixture/rename-request "rename/a.clj" ":your-key" 12 15))))

  (testing "Renaming require alias"
    (h/assert-submap
      {:changes
       {(keyword (h/source-path->uri "rename/a.clj"))
        [{:range {:start {:line 1 :character 36} :end {:line 1 :character 37}}
          :newText "spec"}
         {:range {:start {:line 10 :character 1} :end {:line 10 :character 6}}
          :newText "spec/def"}]}}
      (lsp/request! (fixture/rename-request "rename/a.clj" "spec" 1 36))))

  (testing "Renaming require refer"
    (h/assert-submap
      {:changes
       {(keyword (h/source-path->uri "rename/b.clj"))
        [{:range {:start {:line 5 :character 1} :end {:line 5 :character 8}}
          :newText "your-func"}
         {:range {:start {:line 3 :character 3} :end {:line 3 :character 10}}
          :newText "your-func"}
         {:range {:start {:line 1 :character 36} :end {:line 1 :character 43}}
          :newText "your-func"}]
        (keyword (h/source-path->uri "rename/a.clj"))
        [{:range {:start {:line 3 :character 6} :end {:line 3 :character 13}}
          :newText "your-func"}
         {:range {:start {:line 6 :character 1} :end {:line 6 :character 8}}
          :newText "your-func"}
         {:range {:start {:line 8 :character 1} :end {:line 8 :character 8}}
          :newText "your-func"}]}}
      (lsp/request! (fixture/rename-request "rename/b.clj" "your-func" 1 40)))))

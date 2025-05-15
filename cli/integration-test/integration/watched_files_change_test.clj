(ns integration.watched-files-change-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [integration.fixture :as fixture]
   [integration.helper :as h]
   [integration.lsp :as lsp]))

(def a-file-path "watched_files_change/a.clj")
(def b-file-path "watched_files_change/b.clj")

(lsp/clean-after-test)

(deftest watched-file-removed
  (lsp/start-process!)
  (lsp/request! (fixture/initialize-request))
  (lsp/notify! (fixture/initialized-notification))
  (lsp/notify! (fixture/did-open-source-path-notification b-file-path))

  (testing "Before removal, there is a diagnostic in the file to be removed"
    (is (= 1 (count (lsp/client-awaits-server-diagnostics b-file-path)))))

  (testing "Before removal, the referenced file has usages"
    (h/assert-submaps
      [{:uri (h/source-path->uri b-file-path)}
       {:uri (h/source-path->uri a-file-path)}]
      (lsp/request! (fixture/references-request a-file-path 2 6))))

  (lsp/notify! (fixture/did-change-watched-files [[b-file-path :deleted]]))

  (testing "After removal, there is no diagnostics for the deleted file"
    (h/assert-submaps
      []
      (lsp/client-awaits-server-diagnostics b-file-path)))

  (testing "After removal, the reference files are re-analyzed"
    (is (= 0 (count (lsp/client-awaits-server-diagnostics a-file-path)))))

  (testing "After removal, the references are updated"
    (h/assert-submaps
      [{:uri (h/source-path->uri a-file-path)}]
      (lsp/request! (fixture/references-request a-file-path 2 6)))))

(ns integration.cursor-info-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [integration.fixture :as fixture]
   [integration.helper :as h]
   [integration.lsp :as lsp]))

(lsp/clean-after-test)

(deftest cursor-info
  (lsp/start-process!)
  (lsp/request! (fixture/initialize-request))
  (lsp/notify! (fixture/initialized-notification))
  (lsp/notify! (fixture/did-open-notification "cursor_info/a.clj"))

  (testing "cursor info on empty line"
    (h/assert-submap
      {:elements []}
      (lsp/request! (fixture/cursor-info-raw-request "cursor_info/a.clj" 1 2))))

  (testing "cursor info on local var"
    (let [definition-bucket (-> (fixture/cursor-info-raw-request "cursor_info/a.clj" 3 2)
                                (lsp/request!)
                                (get-in [:elements 0 :definition :bucket]))]
      (is (= "locals" definition-bucket))))

  (testing "cursor info on definition"
    (let [first-element (-> (fixture/cursor-info-raw-request "cursor_info/a.clj" 2 8)
                            (lsp/request!)
                            (get-in [:elements 0]))
          element (:element first-element)
          definition (:definition first-element)]
      (is (not (nil? first-element)))
      (is (= (:uri element) (:uri definition)))
      (is (= (:name-row element) (:name-row definition)))
      (is (= (:name-col element) (:name-col definition))))))

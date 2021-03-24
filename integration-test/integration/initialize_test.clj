(ns integration.initialize-test
  (:require
    [clojure.test :refer [deftest testing]]
    [integration.fixture :as fixture]
    [integration.helper :as h]))

(deftest initialize
  (h/start-process!)
  (testing "initialize request"
    (h/request! (fixture/initialize-request))

    (h/assert-submap
      {"id" @fixture/request-id}
      (h/await-response! @fixture/request-id)))


  (testing "initialized notification"
    (h/request! (fixture/initialized-request))

    (h/assert-submap
      {"id" "1"
       "method" "client/registerCapability"}
      (h/await-response! "1"))))

(ns integration.initialize-test
  (:require
    [cheshire.core :as json]
    [clojure.string :as string]
    [clojure.test :refer [deftest is testing]]
    [integration.common :as common]))

(def initialize-request
  (json/generate-string
    {:jsonrpc "2.0"
     :method :initialize
     :params {:rootUri (str "file://" common/root-project-path)}
     :id 1}))

(def initialized-request
  (json/generate-string
    {:jsonrpc "2.0"
     :method :initialized
     :params {}
     :id 2}))

(deftest initialize
  (common/start-process!)
  (testing "initialize request"
    (println "sending initialize request with body:" initialize-request)
    (binding [*out* common/*stdin*]
      (println (str "Content-Length: " (common/content-length initialize-request)))
      (println "")
      (println initialize-request))

    (binding [*in* common/*stdout*]
      (is (string/starts-with? (read-line) "Content-Length: "))
      (is "" (read-line))
      (common/assert-submap
        {"id" 1}
        (cheshire.core/parse-stream *in*))))

  (testing "initialized notification"
    (println "sending initialized request with body:" initialized-request)
    (binding [*out* common/*stdin*]
      (println (str "Content-Length: " (common/content-length initialized-request)))
      (println "")
      (println initialized-request))

    (binding [*in* common/*stdout*]
      (is (string/starts-with? (read-line) "Content-Length: "))
      (is "" (read-line))
      (common/assert-submap
        {"id" "1"
         "method" "client/registerCapability"}
        (cheshire.core/parse-stream *in*)))))

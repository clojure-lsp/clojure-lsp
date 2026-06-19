(ns clojure-lsp.refactor.command-test
  (:require
   [clojure-lsp.feature.command :as f.command]
   [clojure-lsp.test-helper.internal :as h]
   [clojure.test :refer [deftest is testing]]))

(deftest command-arguments
  (testing "invoke command with 4 arguments, expecting second two to be cursor location"
    (with-redefs [f.command/run-command (fn [arg]
                                          (is (and (= ["new-fn"] (:args arg))
                                                   (= 24 (:row arg))
                                                   (= 37 (:col arg))
                                                   (= 24 (:end-row arg))
                                                   (= 37 (:end-col arg))))
                                          {:no-op? true})]
      (f.command/call-command :extract-function ["file:///a.clj" 23 36 "new-fn"] {:db* (h/db*)})))
  (testing "invoke command with 6 arguments, expecting second two and last two to be cursor locations"
    (with-redefs [f.command/run-command (fn [arg]
                                          (is (and (= ["new-fn"] (:args arg))
                                                   (= 24 (:row arg))
                                                   (= 37 (:col arg))
                                                   (= 25 (:end-row arg))
                                                   (= 38 (:end-col arg)))) {:no-op? true})]
      (f.command/call-command :extract-function ["file:///a.clj" 23 36 "new-fn" 24 37] {:db* (h/db*)}))))
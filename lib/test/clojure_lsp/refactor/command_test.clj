(ns clojure-lsp.refactor.command-test
  (:require
   [clojure-lsp.feature.add-missing-libspec :as f.add-missing-libspec]
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
(deftest add-missing-import-arguments
  (let [calls (atom [])]
    (with-redefs [f.add-missing-libspec/add-missing-import (fn [_loc _uri import-name _db _components]
                                                             (swap! calls conj import-name)
                                                             nil)]
      (testing "without the optional class name, the name is nil and the server looks it up"
        (#'f.command/run-command {:command :add-missing-import :args []})
        (is (= [nil] @calls)))
      (testing "with a class name, it is passed through"
        (reset! calls [])
        (#'f.command/run-command {:command :add-missing-import :args ["java.util.Date"]})
        (is (= ["java.util.Date"] @calls)))
      (testing "the deprecated command behaves the same"
        (reset! calls [])
        (#'f.command/run-command {:command :add-import-to-namespace :args []})
        (is (= [nil] @calls))))))

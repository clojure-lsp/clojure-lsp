(ns clojure-lsp.refactor.command-test
  (:require
   [babashka.fs :as fs]
   [clojure-lsp.feature.command :as f.command]
   [clojure-lsp.test-helper.internal :as h]
   [clojure.java.io :as io]
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

(defn ^:private call-command-at-cursor
  "Loads `code` and calls `command` at its `|` cursor, followed by `extra-args`."
  [command code & extra-args]
  (let [[[row col]] (h/load-code-and-locs code)]
    (f.command/call-command command
                            (into [h/default-uri (dec row) (dec col)] extra-args)
                            (h/components))))

(defn ^:private edit-new-texts [result]
  (map :new-text (get-in result [:edit :changes h/default-uri])))

(deftest add-missing-import-command-test
  (h/reset-components!)
  (swap! (h/db*) assoc-in [:settings :clean :automatically-after-ns-refactor] false)
  (h/load-java-path (str (fs/canonicalize (io/file "test" "fixtures" "java_interop" "File.java"))))
  (let [expected-ns (h/code "(ns foo.bar "
                            "  (:import"
                            "    [java.io File]))")]
    (testing "without import name, imports the class under the cursor"
      (is (= [expected-ns]
             (edit-new-texts (call-command-at-cursor :add-missing-import "(ns foo.bar) |File.")))))
    (testing "without import name and an unknown class, returns nothing to change"
      (h/assert-submap {:error {:message "Nothing to change."}}
                       (call-command-at-cursor :add-missing-import "(ns foo.bar) |MyClass.")))
    (testing "with import name"
      (is (= [expected-ns]
             (edit-new-texts (call-command-at-cursor :add-missing-import "(ns foo.bar) |File." "java.io.File")))))
    (testing "deprecated add-import-to-namespace without import name"
      (is (= [expected-ns]
             (edit-new-texts (call-command-at-cursor :add-import-to-namespace "(ns foo.bar) |File.")))))))
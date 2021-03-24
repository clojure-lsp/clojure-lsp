(ns integration.helper
  (:require
   [clojure.java.io :as io]
   [clojure.pprint :as pprint]
   [clojure.test :refer [is use-fixtures]]
   [integration.lsp :as lsp]))

(def root-project-path
  (-> (io/file *file*)
      .getParentFile
      .getParentFile
      .toPath
      (.resolve "sample-test")
      str))

(defn clean-after-test []
  (use-fixtures :each (fn [f] (lsp/clean!) (f)))
  (use-fixtures :once (fn [f] (f) (lsp/clean!))))

(defn assert-submap [expected actual]
  (is (= expected
         (some-> actual (select-keys (keys expected))))
      (str "No superset of " (pr-str actual) " found")))

(defmacro assert-submaps
  "Asserts that maps are submaps of result in corresponding order and
  that the number of maps corresponds to the number of
  results. Returns true if all assertions passed (useful for REPL).

   taken from kondo"
  [maps result]
  `(let [maps# ~maps
         res# ~result]
     (and
      (is (= (count maps#) (count res#))
          (format "Expected %s results, but got: %s \n--\n%s--"
                  (count maps#) (count res#) (with-out-str (pprint/pprint res#))))
      (doseq [[r# m#] (map vector res# maps#)]
        (assert-submap m# r#)))))

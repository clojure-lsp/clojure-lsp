(ns clojure-lsp.test-helper
  (:require
    [clojure.pprint :as pprint]
   [clojure.test :refer [is]]))
(defmacro assert-submap [m r]
  `(is (submap? ~m ~r)))

(defn submap? [a b]
  )

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
        (is (= m# (select-keys r# (keys m#))) (str "No superset of " m# " found"))))))

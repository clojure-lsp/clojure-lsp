(ns clojure-lsp.features.linked-editing-range-test
  (:require
   [clojure-lsp.feature.linked-editing-range :as f.linked-editing-range]
   [clojure-lsp.test-helper :as h]
   [clojure.test :refer [deftest testing]]))

(deftest ranges
  (let [components (h/make-components)
        [defn-start defn-end
         [usage-1-r usage-1-c :as usage-1-start] usage-1-end
         usage-2-start usage-2-end]
        (h/load-code
          (h/code "(ns foo)"
                  ":foo"
                  "(defn |foo| [a] a)"
                  "(defn some-fn [a] a)"
                  "(|foo| 1)"
                  "some-fn"
                  "|foo|") h/default-uri components)]
    (h/load-code
      (h/code "(ns bar"
              " (:require [foo :as f]))"
              "(f/some-fn 3)"
              "") "file:///b.clj" components)
    (let [db (h/db components)]
      (testing "when all references are on the same file"
        (h/assert-submap
          {:ranges [(h/->range defn-start defn-end)
                    (h/->range usage-1-start usage-1-end)
                    (h/->range usage-2-start usage-2-end)]}
          (f.linked-editing-range/ranges (h/file-uri "file:///a.clj") usage-1-r usage-1-c db)))
      (testing "when some reference is on another file"
        (h/assert-submap
          {:error {:code :invalid-params
                   :message "There are references on other files for this symbol"}}
          (f.linked-editing-range/ranges (h/file-uri "file:///a.clj") 6 2 db))))))

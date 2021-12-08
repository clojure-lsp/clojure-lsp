(ns clojure-lsp.feature.test-tree
  (:require
   [clojure-lsp.shared :as shared]))

(defn ^:private ->testings-children [testings]
  (let [root-testings (remove (fn [testing]
                                (some #(and (shared/inside? testing %)
                                            (not= % testing)) testings))
                              testings)]
    (mapv (fn [root-testing]
            (let [inside-testings (filter (fn [testing]
                                            (and (shared/inside? testing root-testing)
                                                 (not= testing root-testing)))
                                          testings)]
              (shared/assoc-some
                {:name "mock"
                 :range (shared/->scope-range root-testing)
                 :name-range (shared/->range root-testing)
                 :kind :testing}
                :children (when (seq inside-testings) (->testings-children inside-testings)))))
          root-testings)))

(defn ^:private deftest->tree [deftest testings]
  (let [local-testings (filter #(shared/inside? % deftest) testings)]
    {:name (str (:name deftest))
     :range (shared/->scope-range deftest)
     :name-range (shared/->range deftest)
     :kind :deftest
     :children (->testings-children local-testings)}))

(defn tree [uri db]
  (let [filename (shared/uri->filename uri)
        local-analysis (get-in @db [:analysis filename])
        deftests (->> local-analysis
                      (filter #(and (identical? :var-definitions (:bucket %))
                                    (contains? '#{clojure.test/deftest cljs.test/deftest}
                                               (:defined-by %)))))
        testings (->> local-analysis
                      (filter #(and (identical? :var-usages (:bucket %))
                                    (= 'testing (:name %))
                                    (contains? '#{clojure.test cljs.test} (:to %)))))
        tests-tree (mapv #(deftest->tree % testings) deftests)]
    (when (seq tests-tree)
      {:uri uri
       :tests (mapv #(deftest->tree % testings) deftests)})))

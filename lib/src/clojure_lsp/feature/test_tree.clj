(ns clojure-lsp.feature.test-tree
  (:require
   [clojure-lsp.queries :as q]
   [clojure-lsp.shared :as shared]))

(set! *warn-on-reflection* true)

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
                {:name (str
                         (or (-> root-testing :context :clojure.test :testing-str) ;; not always a string
                             ""))
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
  (let [ns-element (q/find-namespace-definition-by-uri db uri)
        local-buckets (get-in db [:analysis uri])
        deftests (into []
                       (filter #(contains? '#{clojure.test/deftest cljs.test/deftest}
                                           (:defined-by %)))
                       (:var-definitions local-buckets))
        testings (into []
                       (filter #(and (= 'testing (:name %))
                                     (contains? '#{clojure.test cljs.test} (:to %))))
                       (:var-usages local-buckets))
        tests-tree (mapv #(deftest->tree % testings) deftests)]
    (when (seq tests-tree)
      {:uri uri
       :tree {:name (str (:name ns-element))
              :range (shared/->scope-range ns-element)
              :name-range (shared/->range ns-element)
              :kind :namespace
              :children tests-tree}})))

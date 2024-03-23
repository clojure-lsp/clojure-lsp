(ns clojure-lsp.feature.project-tree
  (:require
   [clojure-lsp.queries :as q]
   [clojure-lsp.shared :as shared]
   [clojure.java.io :as io]))

(defn ^:private root-node [db]
  (let [source-paths (:source-paths (:settings db))
        project-root (io/file (str (shared/uri->path (:project-root-uri db))))]
    {:name (.getName project-root)
     :type :project
     :nodes (concat
              (mapv (fn [source-path]
                      {:name (shared/relativize-filepath source-path (.getCanonicalPath project-root))
                       :final false
                       :type :source-path})
                    (sort source-paths))
              [{:name "External dependencies"
                :id :external-dependencies
                :final false
                :type :library}])}))

(defn ^:private source-path-node [node db]
  (let [source-path-uri (shared/filename->uri (shared/absolute-path (:name node) db) db)
        namespace-definitions (q/find-all-path-namespace-definitions db source-path-uri)]
    {:name (:name node)
     :type (:type node)
     :nodes (mapv
              (fn [namespace-definition]
                {:name (str (:name namespace-definition))
                 :uri (:uri namespace-definition)
                 :final false
                 :type :ns})
              (sort-by :name namespace-definitions))}))

(defn ^:private external-dependencies-node [node db]
  (let [uris (keys (q/external-analysis db))
        jars (set (keep (fn [uri]
                          (second (re-find #"(.+)!/|::.*" uri)))
                        uris))]
    {:name (:name node)
     :type (:type node)
     :id (:id node)
     :nodes (mapv
              (fn [jar-uri]
                {:name (second (re-find (re-pattern (str ".+" (if shared/windows-os?
                                                                (str "\\" (System/getProperty "file.separator"))
                                                                (System/getProperty "file.separator")) "(.+.jar$)")) jar-uri))
                 :detail jar-uri
                 :uri jar-uri
                 :final false
                 :type :jar})
              jars)}))

(defn ^:private jar-node [node db]
  (let [ns-definitions (q/find-all-path-namespace-definitions db (:uri node))
        java-class-definitions (q/find-all-path-java-class-definitions db (:uri node))]
    {:name (:name node)
     :type (:type node)
     :detail (:detail node)
     :uri (:uri node)
     :nodes (keep
              (fn [element]
                (case (:bucket element)
                  :namespace-definitions
                  {:name (str (:name element))
                   :uri (:uri element)
                   :final false
                   :type :ns}

                  :java-class-definitions
                  {:name (str (:class element))
                   :uri (:uri element)
                   :final true
                   :type :class}))
              (concat ns-definitions java-class-definitions))}))

(defn ^:private ns-node [node db]
  (let [definitions (concat (q/find-var-definitions db (:uri node) true)
                            (q/find-keyword-definitions db (:uri node)))]
    {:name (:name node)
     :type (:type node)
     :uri (:uri node)
     :nodes (mapv
              (fn [element]
                (shared/assoc-some
                  {:name (str (:name element))
                   :uri (:uri element)
                   :range (shared/->range element)
                   :final true
                   :type (q/element->symbol-kind element)}
                  :detail (cond (:private element) "private"
                                (:reg element) (name (:reg element)))))
              definitions)}))

(defn nodes [db current-node]
  (cond
    (not current-node)
    (root-node db)

    (= :source-path (:type current-node))
    (source-path-node current-node db)

    (and (= :library (:type current-node))
         (= "external-dependencies" (:id current-node)))
    (external-dependencies-node current-node db)

    (= :jar (:type current-node))
    (jar-node current-node db)

    (= :ns (:type current-node))
    (ns-node current-node db)))

(comment
  ;; workspace/projectTree/nodes - no args
  {:name "clojure_sample"
   :type :project
   :nodes [{:name "src/main/clojure"
            :uri "/user/project/src/main/clojure"
            :final false
            :type :source-path}
           {:name "test/main/clojure"
            :final false
            :type :source-path}
           {:name "External dependencies"
            :id "external-dependencies"
            :final false
            :type :library}]}

  ;; workspace/projectTree/nodes - source-path

  {:nodes [{:name "foo.bar"
            :uri "file:///..."
            :final false
            :type :ns}]}

  ;; workspace/projectTree/nodes - external-dependencies

  {:nodes [{:name "guava.jar"
            :uri "jar://...."
            :final false
            :type :jar}]}

  ;; workspace/projectTree/nodes - jar with uri X

  {:nodes [{:name "bla.Foo"
            :uri "jar://...."
            :detail "jar://...."
            :final true
            :type :class}]}
  ;; or
  {:nodes [{:name "foo.bar"
            :uri "jar://...."
            :final false
            :type :ns}]}

  ;; workspace/projectTree/nodes - ns

  {:nodes [{:name "some-function"
            :uri "file:///..."
            :range {:start {:line 1 :character 2}
                    :end {:line 2 :character 3}}
            :final true
            :type :function
            ;; or :type :variable
            ;; or :type :class
            ;; or :type :interface
            }]})

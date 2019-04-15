(require '[clojure.edn :as edn])

(def +deps+ (-> "deps.edn" slurp edn/read-string))

(defn deps->vec [deps]
  (mapv (fn [[dep {:keys [:mvn/version exclusions]}]]
          (cond-> [dep version]
            exclusions (conj :exclusions exclusions)))
        deps))

(def dependencies
  (deps->vec (:deps +deps+)))

(defproject clojure-lsp "0.1.0-SNAPSHOT"
  :dependencies ~dependencies
  :jvm-opts ^:replace ["-Xmx1g" "-server"]
  :main clojure-lsp.main
  :java-source-paths ["src-java"]
  :profiles {:dev {:plugins [[com.jakemccrary/lein-test-refresh "0.23.0"]
                             [lein-binplus "0.6.5"]]
                   :bin {:name "clojure-lsp"}}
             :test {:test-selectors {:focused :focused}}
             :uberjar {:aot :all}})

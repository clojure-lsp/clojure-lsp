(require '[clojure.edn :as edn])

(def +deps+ (-> "deps.edn" slurp edn/read-string))

(defn deps->vec [deps]
  (keep (fn [[dep {:keys [:mvn/version exclusions]}]]
          (when version
            (cond-> [dep version]
              exclusions (conj :exclusions exclusions))))
        deps))

(def dependencies
  (deps->vec (:deps +deps+)))

(defproject clojure-lsp "0.1.0-SNAPSHOT"
  :dependencies ~dependencies
  :jvm-opts ^:replace ["-Xmx1g" "-server"]
  :main clojure-lsp.main
  :java-source-paths ["src-java"]
  :resource-paths ["resources" "lib/rewrite-clj-0.6.2-SNAPSHOT.jar"]
  :profiles {:dev {:plugins [[com.jakemccrary/lein-test-refresh "0.23.0"]
                             [lein-binplus "0.6.5"]]
                   :bin {:name "clojure-lsp"}}
             :test {:test-selectors {:focused :focused}}
             :uberjar {:aot :all}})

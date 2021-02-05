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

(defproject clojure-lsp
  #=(clojure.string/trim
      #=(slurp "resources/CLOJURE_LSP_VERSION"))
  :dependencies ~dependencies
  :jvm-opts ^:replace ["-Xmx2g" "-server"]
  :main clojure-lsp.main
  :java-source-paths ["src-java"]
  :resource-paths ["resources"]
  :javac-options ["-implicit:none"]
  :bin {:name "clojure-lsp"}
  :profiles {:dev {:plugins [[com.jakemccrary/lein-test-refresh "0.24.1"]
                             [lein-binplus "0.6.6"]]}
             :debug ^:leaky {:dependencies [[nrepl/nrepl "0.8.3"]]}
             :test {:test-selectors {:focused :focused}
                    :dependencies [[pjstadig/humane-test-output "0.9.0"]]
                    :injections [(require 'pjstadig.humane-test-output)
                                 (pjstadig.humane-test-output/activate!)]}
             :native-image {:dependencies [[ericdallo/sqlite-jni-graal-fix "0.0.2-graalvm-21.0.0"]]
                            :jvm-opts ["-Xmx2g"
                                       "-server"
                                       "-Dclojure.compiler.direct-linking=true"
                                       "-Dclojure.spec.skip-macros=true"]}
             :uberjar {:aot :all
                       :jvm-opts ["-Xmx2g"
                                  "-server"]}})

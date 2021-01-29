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

(defproject clojure-lsp :project/git-tag
  :dependencies ~dependencies
  :jvm-opts ^:replace ["-Xmx2g" "-server"]
  :main clojure-lsp.main
  :java-source-paths ["src-java"]
  :resource-paths ["resources"]
  :javac-options ["-implicit:none"]
  :plugins [[me.arrdem/lein-git-version "2.0.8"]]
  :git-version {:status-to-version
                (fn [{:keys [tag]}]
                  tag)}
  :profiles {:dev {:plugins [[com.jakemccrary/lein-test-refresh "0.24.1"]
                             [lein-binplus "0.6.6"]]
                   :bin {:name "clojure-lsp"}}
             :test {:test-selectors {:focused :focused}
                    :dependencies [[pjstadig/humane-test-output "0.9.0"]]
                    :injections [(require 'pjstadig.humane-test-output)
                                 (pjstadig.humane-test-output/activate!)]}
             :native-image {:dependencies [[ericdallo/sqlite-jni-graal-fix "0.0.2-graalvm-20.2.0"]
                                           [org.graalvm.nativeimage/svm "20.2.0"]]}
             :uberjar {:aot :all
                       :jvm-opts ["-server"
                                  "-Dclojure.compiler.direct-linking=true"
                                  "-Dclojure.spec.skip-macros=true"]}})

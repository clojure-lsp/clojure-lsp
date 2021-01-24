#!/usr/bin/env bash

GRAAL_VM_DOCKER_IMAGE=springci/graalvm-ce:20.2-dev-java11

LEIN_SNAPSHOTS_IN_RELEASE=1 lein with-profiles +clojure-1.10.2,+native-image "do" clean, uberjar

jar=$(ls target/clojure-lsp-*-standalone.jar)

# outfile="/clojure-lsp/$jar"
outfile="$jar"

args=( "-jar" "$outfile"
              "-H:Name=clojure-lsp"
              "-H:+ReportExceptionStackTraces"
              "-J-Dclojure.spec.skip-macros=true"
              "-J-Dclojure.compiler.direct-linking=true"
              # "-H:ReflectionConfigurationFiles=/clojure-lsp/graalvm/reflection.json"
              "-H:ReflectionConfigurationFiles=graalvm/reflection.json"
              "--initialize-at-build-time"
              # "--initialize-at-run-time=org.apache.log4j.LogManager"
              "-H:+TraceClassInitialization"
              "-H:IncludeResources='db/.*|static/.*|templates/.*|.*.yml|.*.xml|.*/org/sqlite/.*|org/sqlite/.*|.*.xml|.*.conf'"
              "--report-unsupported-elements-at-runtime"
              "-H:Log=registerResource:"
              "--initialize-at-run-time=io.netty.util.internal.logging.Log4JLogger"
              "--allow-incomplete-classpath"
              "--verbose"
              "--no-fallback"
              "--no-server"
              "--static"
              "-J-Xmx3g" )

# docker run --rm -v ${PWD}:/clojure-lsp $GRAAL_VM_DOCKER_IMAGE native-image "${args[@]}"
native-image "${args[@]}"

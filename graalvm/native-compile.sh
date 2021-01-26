#!/usr/bin/env bash

# GRAAL_VM_DOCKER_IMAGE=springci/graalvm-ce:20.3-dev-java11
GRAAL_VM_DOCKER_IMAGE=springci/graalvm-ce:21.0-dev-java11
LEIN_SNAPSHOTS_IN_RELEASE=1 lein with-profiles +native-image "do" clean, uberjar

native-image \
    -jar target/*-standalone.jar \
    -H:Name=clojure-lsp \
    -J-Dclojure.compiler.direct-linking=true \
    -J-Dclojure.spec.skip-macros=true \
    -H:+ReportExceptionStackTraces \
    -H:Log=registerResource: \
    --verbose \
    -H:IncludeResources="db/.*|static/.*|templates/.*|.*.yml|.*.xml|.*/org/sqlite/.*|org/sqlite/.*|.*.properties" \
    -H:ConfigurationFileDirectories=graalvm/new \
    --initialize-at-build-time  \
    --report-unsupported-elements-at-runtime \
    --allow-incomplete-classpath \
    --no-server \
    --no-fallback \
    -J-Xmx4g

    #--static \
        # -H:ReflectionConfigurationFiles=graalvm/new/reflect-config.json \
    # -H:DynamicProxyConfigurationFiles=graalvm/new/proxy-config.json \
    # -H:JNIConfigurationFiles=graalvm/new/jni-config.json

# jar=$(ls target/clojure-lsp-*-standalone.jar)

# outfile="/clojure-lsp/$jar"

# args=( "-jar" "$outfile"
#               "-H:Name=/clojure-lsp/clojure-lsp"
#               "-H:+ReportExceptionStackTraces"
#               "-J-Dclojure.spec.skip-macros=true"
#               "-J-Dclojure.compiler.direct-linking=true"
#               "-H:ReflectionConfigurationFiles=/clojure-lsp/graalvm/reflection.json"
#               "-H:DynamicProxyConfigurationFiles=/clojure-lsp/graalvm/proxy.json"
#               "--initialize-at-build-time"
#               # "-H:TraceClassInitialization="
#               "-H:IncludeResources='db/.*|static/.*|templates/.*|.*.yml|.*.xml|.*/org/sqlite/.*|org/sqlite/.*'"
#               "--report-unsupported-elements-at-runtime"
#               "-H:Log=registerResource:"
#               "--initialize-at-run-time=io.netty.util.internal.logging.Log4JLogger"
#               "--allow-incomplete-classpath"
#               "--verbose"
#               "--no-fallback"
#               "--no-server"
#               "--static"
#               "-J-Xmx3g" )

# docker run --rm -v ${PWD}:/clojure-lsp $GRAAL_VM_DOCKER_IMAGE native-image "${args[@]}"

# args=(
#     "-agentlib:native-image-agent=config-output-dir=/clojure-lsp/graalvm/new"
#     "-jar" "$outfile" 
# )

# docker run -i --rm -v ${PWD}:/clojure-lsp $GRAAL_VM_DOCKER_IMAGE java "${args[@]}" 

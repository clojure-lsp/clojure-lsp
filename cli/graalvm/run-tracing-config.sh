#!/usr/bin/env bash

clojure -X:cli-prod-jar
GRAAL_VM_DOCKER_IMAGE=springci/graalvm-ce:20.2-dev-java11

jar=$(ls clojure-lsp.jar)

outfile="/clojure-lsp/$jar"

args=(
    "-agentlib:native-image-agent=config-merge-dir=/clojure-lsp/graalvm"
    "-jar" "$outfile"
)

docker run -i --rm -v ${PWD}:/clojure-lsp $GRAAL_VM_DOCKER_IMAGE java "${args[@]}"

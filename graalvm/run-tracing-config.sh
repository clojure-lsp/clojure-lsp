#!/usr/bin/env bash

lein "do" clean, uberjar
GRAAL_VM_DOCKER_IMAGE=springci/graalvm-ce:20.2-dev-java11

jar=$(ls target/clojure-lsp-*-standalone.jar)

outfile="/clojure-lsp/$jar"

args=(
    "-agentlib:native-image-agent=config-merge-dir=/clojure-lsp/graalvm"
    "-jar" "$outfile"
)

docker run -i --rm -v ${PWD}:/clojure-lsp $GRAAL_VM_DOCKER_IMAGE java "${args[@]}"

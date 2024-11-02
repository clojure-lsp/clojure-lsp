#!/usr/bin/env bash

java -agentlib:native-image-agent=config-merge-dir=/home/greg/dev/clojure-lsp/cli/graalvm -jar /home/greg/dev/clojure-lsp/cli/clojure-lsp.jar

#!/usr/bin/env bash

# Deprecaed script, use make native-cli

set -e

if [ -z "$GRAALVM_HOME" ]; then
    echo "Please set GRAALVM_HOME"
    exit 1
fi

if [[ ! -f "$CLOJURE_LSP_JAR" ]]
then
    make cli-jar
    CLOJURE_LSP_JAR=$(ls clojure-lsp-standalone.jar)
fi

CLOJURE_LSP_XMX=${CLOJURE_LSP_XMX:-"-J-Xmx8g"}

args=("-jar" "$CLOJURE_LSP_JAR"
      "clojure-lsp"
      "-H:+ReportExceptionStackTraces"
      "--verbose"
      "--no-fallback"
      "--native-image-info"
      "$CLOJURE_LSP_XMX")

CLOJURE_LSP_STATIC=${CLOJURE_LSP_STATIC:-}
CLOJURE_LSP_MUSL=${CLOJURE_LSP_MUSL:-}

if [ "$CLOJURE_LSP_STATIC" = "true" ]; then
    args+=("--static")
    if [ "$CLOJURE_LSP_MUSL" = "true" ]; then
        args+=("--libc=musl"
               # see https://github.com/oracle/graal/issues/3398
               "-H:CCompilerOption=-Wl,-z,stack-size=2097152")
    else
        # see https://github.com/oracle/graal/issues/3737
        args+=("-H:+StaticExecutableWithDynamicLibC")
    fi
fi

"$GRAALVM_HOME/bin/native-image" "${args[@]}"

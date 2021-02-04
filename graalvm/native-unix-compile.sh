#!/usr/bin/env bash

set -e

if [ -z "$GRAALVM_HOME" ]; then
    echo "Please set GRAALVM_HOME"
    exit 1
fi

if [[ ! -f "$CLOJURE_LSP_JAR" ]]
then
    lein with-profiles +native-image "do" clean, uberjar
    CLOJURE_LSP_JAR=$(ls target/clojure-lsp-*-standalone.jar)
fi

CLOJURE_LSP_XMX=${CLOJURE_LSP_XMX:-"-J-Xmx4g"}

args=("-jar" "$CLOJURE_LSP_JAR"
      "-H:Name=clojure-lsp"
      "-J-Dclojure.compiler.direct-linking=true"
      "-J-Dclojure.spec.skip-macros=true"
      "-H:+ReportExceptionStackTraces"
      "--enable-url-protocols=jar"
      "-H:+InlineBeforeAnalysis"
      "-H:Log=registerResource:"
      "--verbose"
      "-H:IncludeResources=\"CLOJURE_LSP_VERSION|db/.*|static/.*|templates/.*|.*.yml|.*.xml|.*/org/sqlite/.*|org/sqlite/.*|.*.properties\""
      "-H:ConfigurationFileDirectories=graalvm"
      "--initialize-at-build-time"
      "--report-unsupported-elements-at-runtime"
      "--no-server"
      "--no-fallback"
      "--native-image-info"
      "--allow-incomplete-classpath"
      "-H:ServiceLoaderFeatureExcludeServices=javax.sound.sampled.spi.AudioFileReader"
      "-H:ServiceLoaderFeatureExcludeServices=javax.sound.midi.spi.MidiFileReader"
      "-H:ServiceLoaderFeatureExcludeServices=javax.sound.sampled.spi.MixerProvider"
      "-H:ServiceLoaderFeatureExcludeServices=javax.sound.sampled.spi.FormatConversionProvider"
      "-H:ServiceLoaderFeatureExcludeServices=javax.sound.sampled.spi.AudioFileWriter"
      "-H:ServiceLoaderFeatureExcludeServices=javax.sound.midi.spi.MidiDeviceProvider"
      "-H:ServiceLoaderFeatureExcludeServices=javax.sound.midi.spi.SoundbankReader"
      "-H:ServiceLoaderFeatureExcludeServices=javax.sound.midi.spi.MidiFileWriter"
      "$CLOJURE_LSP_XMX")

CLOJURE_LSP_STATIC=${CLOJURE_LSP_STATIC:-}

if [ "$CLOJURE_LSP_STATIC" = "true" ]; then
    args+=("--static")
fi

"$GRAALVM_HOME/bin/native-image" "${args[@]}"

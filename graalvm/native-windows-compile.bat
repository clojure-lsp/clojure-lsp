@echo off

echo Building clojure-lsp %CLOJURE_LSP_JAR% with Xmx of %CLOJURE_LSP_XMX%

rem the --no-server option is not supported in GraalVM Windows.
call %GRAALVM_HOME%\bin\native-image.cmd ^
      "-jar" "%CLOJURE_LSP_JAR%" ^
      "-H:Name=clojure-lsp" ^
      "-J-Dclojure.compiler.direct-linking=true" ^
      "-J-Dclojure.spec.skip-macros=true" ^
      "-H:+ReportExceptionStackTraces" ^
      "--enable-url-protocols=jar" ^
      "-H:+InlineBeforeAnalysis" ^
      "-H:Log=registerResource:" ^
      "--verbose" ^
      "-H:IncludeResources=db/.*|static/.*|templates/.*|.*.yml|.*.xml|.*/org/sqlite/.*|org/sqlite/.*|.*.properties" ^
      "-H:ConfigurationFileDirectories=graalvm" ^
      "--initialize-at-build-time" ^
      "--report-unsupported-elements-at-runtime" ^
      "--no-fallback" ^
      "--allow-incomplete-classpath" ^
      "-H:ServiceLoaderFeatureExcludeServices=javax.sound.sampled.spi.AudioFileReader" ^
      "-H:ServiceLoaderFeatureExcludeServices=javax.sound.midi.spi.MidiFileReader" ^
      "-H:ServiceLoaderFeatureExcludeServices=javax.sound.sampled.spi.MixerProvider" ^
      "-H:ServiceLoaderFeatureExcludeServices=javax.sound.sampled.spi.FormatConversionProvider" ^
      "-H:ServiceLoaderFeatureExcludeServices=javax.sound.sampled.spi.AudioFileWriter" ^
      "-H:ServiceLoaderFeatureExcludeServices=javax.sound.midi.spi.MidiDeviceProvider" ^
      "-H:ServiceLoaderFeatureExcludeServices=javax.sound.midi.spi.SoundbankReader" ^
      "-H:ServiceLoaderFeatureExcludeServices=javax.sound.midi.spi.MidiFileWriter" ^
      "%CLOJURE_LSP_XMX%"

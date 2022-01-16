@echo off

echo Building clojure-lsp %CLOJURE_LSP_JAR% with Xmx of %CLOJURE_LSP_XMX%

rem the --no-server option is not supported in GraalVM Windows.
call %GRAALVM_HOME%\bin\native-image.cmd ^
      "-jar" "%CLOJURE_LSP_JAR%" ^
      "-H:+ReportExceptionStackTraces" ^
      "--verbose" ^
      "--no-fallback" ^
      "--native-image-info" ^
      "%CLOJURE_LSP_XMX%"

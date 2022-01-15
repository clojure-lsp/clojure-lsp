# Building

## GraalVM

Every release, the native binaries (Windows, Linux and MacOS) are compiled with __GraalVM__ and uploaded to Github releases page.

To build a native image with GraalVM:

- Install the GraalVM 21.3.0 for Java 11 and set the `GRAALVM_HOME` to the installation dir. 
- Install `native-image` with `$GRAALVM_HOME/bin/gu install native-image`. 
- Unix: Run from `clojure-lsp` project root `make native-cli`
- Windows: `cd cli && ./graalvm/native-windows-compile.bat`. 

The build may take some minutes and the result will be a `./clojure-lsp` native binary.

## Jar

### Editor

Run `clojure -X:editor-jar` for building the jar to be used with LSP protocol in a editor or CLI.

### API/CLI

Run `clojure -X:api-jar` for building the jar to be used via CLI or JVM via API.


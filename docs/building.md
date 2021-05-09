# Building

## GraalVM

Every release, the native binaries (Windows, Linux and MacOS) are compiled with __GraalVM__ and uploaded to Github releases page.

To build a native image with GraalVM:

- Install the GraalVM 21.1.0 for Java 11 and set the `GRAALVM_HOME` to the installation dir. 
- Install `native-image` with `$GRAALVM_HOME/bin/gu install native-image`. 
- Run from `clojure-lsp` project root `./graalvm/native-unix-compile.sh` or `./graalvm/native-windows-compile.bat`. 

The build may take some minutes and the result will be a `./clojure-lsp` native binary.

## Jar

Run `clojure -X:prod-jar` for building the jar.


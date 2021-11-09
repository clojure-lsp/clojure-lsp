{ lib, stdenv, graalvm11-ce, gcc, babashka, fetchurl, fetchFromGitHub, clojure
, writeScript, jre, makeWrapper }:

stdenv.mkDerivation rec {
  src = ./.;
  pname = "clojure-lsp";
  version = "master";
  buildInputs = [ gcc graalvm11-ce clojure ];

  # TODO: Build jar instead of fetch
  jar = fetchurl {
    url = "https://github.com/clojure-lsp/clojure-lsp/releases/download/2021.11.02-15.24.47/clojure-lsp.jar";
    sha256 = "sha256-k0mzibcLAspklCPE6f2qsUm9bwSvcJRgWecMBq7mpF0=";
  };

  GRAALVM_HOME = graalvm11-ce;
  CLOJURE_LSP_JAR = jar;
  CLOJURE_LSP_XMX = "-J-Xmx8g";

  buildPhase = ''
    runHook preBuild
    export HOME=$(mktemp -d)
    # FIXME: Failed to read artifact descriptor for org.clojure:clojure:jar:1.10.3
    # make prod-jar-for-native

    DTLV_LIB_EXTRACT_DIR=$(mktemp -d)
    export DTLV_LIB_EXTRACT_DIR=$DTLV_LIB_EXTRACT_DIR

    args=("-jar" "$CLOJURE_LSP_JAR"
          "-H:CLibraryPath=${graalvm11-ce.lib}/lib"
          "-H:CLibraryPath=$DTLV_LIB_EXTRACT_DIR"
          "-H:+ReportExceptionStackTraces"
          "--verbose"
          "--no-fallback"
          "--native-image-info"
          "$CLOJURE_LSP_XMX")
    native-image ''${args[@]}
    runHook postBuild
    unset HOME
  '';

  installPhase = ''
    runHook preInstall
    install -Dm755 ./clojure-lsp $out/bin/clojure-lsp
    runHook postInstall
  '';

  doCheck = true;
  checkPhase = ''
    export HOME=$(mktemp -d)
    runHook preCheck
    # TODO: find a better way to do this
    # ./clojure-lsp --version | fgrep -q '${version}'
    ${babashka}/bin/bb integration-test ./clojure-lsp
    runHook postCheck
    unset HOME
  '';
}

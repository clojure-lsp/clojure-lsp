{ lib, stdenv, graalvm11-ce, babashka, fetchurl, fetchFromGitHub, clojure
, writeScript, jre, makeWrapper }:

stdenv.mkDerivation rec {
  src = ./.;
  pname = "clojure-lsp";
  version = "master";
  buildInputs = [ graalvm11-ce clojure ];

  # TODO: Build jar instead of fetch
  jar = fetchurl {
    url =
      "https://github.com/clojure-lsp/clojure-lsp/releases/download/2021.10.20-13.04.11/clojure-lsp.jar";
    sha256 = "1p2mf1mzxyj9csx4vf1yf37630dpp7q3kl1fdzdkpdaq8j2ax7d9";
  };

  GRAALVM_HOME = graalvm11-ce;
  CLOJURE_LSP_JAR = jar;
  CLOJURE_LSP_XMX = "-J-Xmx4g";

  buildPhase = ''
    export HOME=$(mktemp -d)
    runHook preBuild
    # FIXME: Failed to read artifact descriptor for org.clojure:clojure:jar:1.10.3
    # make prod-jar-for-native

    args=("-jar" "$CLOJURE_LSP_JAR"
          "-H:CLibraryPath=${graalvm11-ce.lib}/lib"
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

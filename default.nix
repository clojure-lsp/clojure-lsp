{ lib, stdenv, graalvm11-ce, gcc, babashka, fetchurl, fetchFromGitHub, clojure
, writeScript, jre, makeWrapper }:

stdenv.mkDerivation rec {
  src = ./.;
  pname = "clojure-lsp";
  version = "master";
  buildInputs = [ gcc graalvm11-ce clojure ];

  GRAALVM_HOME = graalvm11-ce;
  CLOJURE_LSP_XMX = "-J-Xmx8g";

  clojureDependenciesSha256 = "sha256-8seX3pNYvEuA+c89Msgs7tzlbHPwY+uphd7xl7n3MAA=";
  fetchedClojureDeps = stdenv.mkDerivation {
    name = "clojure-lsp-${version}-clojure-deps";
    buildInputs = [ clojure ];
    inherit src;

    buildPhase = ''
      export HOME=$(mktemp -d)
      mkdir -p $out/.m2/repository
      clojure -A:javac:prod-jar-for-native:native -Spath
      cp -rf /build/.m2 $out/
    '';

    installPhase = ''
      find $out -type f \
        -name \*.lastUpdated -or \
        -name resolver-status.properties -or \
        -name _remote.repositories \
        -delete
    '';

    dontFixup = true;
    outputHashAlgo = "sha256";
    outputHashMode = "recursive";
    outputHash = clojureDependenciesSha256;
  };

  CLJ_CONFIG = ''{:mvn/local-repo "${fetchedClojureDeps}/.m2/repository"}'';

  buildPhase = ''
    runHook preBuild
    export HOME=$(mktemp -d)

    ;;TODO working until here
    clojure -Sdeps '{:mvn/local-repo "${fetchedClojureDeps}/.m2/repository"}' -A:javac:prod-jar-for-native:native -X:javac
    clojure -Sdeps '{:mvn/local-repo "${fetchedClojureDeps}/.m2/repository"}' -A:javac:prod-jar-for-native:native -X:prod-jar-for-native

    CLOJURE_LSP_JAR = clojure-lsp.jar;

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

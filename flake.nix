{
  description = "Clojure LSP flake";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    clj-nix = {
      url = "github:jlesquembre/clj-nix?ref=0.2.0";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };
  outputs = { self, nixpkgs, flake-utils, clj-nix }:

    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        cljpkgs = clj-nix.packages."${system}";
      in
      {
        packages = rec {
          default = clojure-lsp;

          clojure-lsp-jdk = cljpkgs.mkCljBin {
            projectSrc = ./.;
            name = "com.github.clojure-lsp/clojure-lsp";
            main-ns = "clojure-lsp.main";
            jdkRunner = pkgs.jdk17_headless;
            buildCommand =
              ''
                mkdir -p target
                make cli-jar-for-native
                cp clojure-lsp-standalone.jar target
              '';
            maven-extra = [{
              content =
                ''
                  <?xml version="1.0" encoding="UTF-8"?>
                  <metadata modelVersion="1.1.0">
                    <groupId>com.google.code.gson</groupId>
                    <artifactId>gson</artifactId>
                    <versioning>
                      <latest>2.9.0</latest>
                      <release>2.9.0</release>
                      <versions>
                        <version>2.8.9</version>
                        <version>2.9.0</version>
                      </versions>
                      <lastUpdated>19700101000000</lastUpdated>
                    </versioning>
                  </metadata>
                '';
              path = "com/google/code/gson/gson/maven-metadata-central.xml";
            }];
          };

          clojure-lsp = cljpkgs.mkGraalBin {
            cljDrv = self.packages."${system}".clojure-lsp-jdk;
          };

        };
      }) // {
        overlays.default = (final: prev: {
          clojure-lsp = self.packages.${final.system}.default;
        });
      };

}

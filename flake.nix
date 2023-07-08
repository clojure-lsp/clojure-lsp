{
  description = "Clojure LSP flake";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    clj-nix = {
      url = "github:jlesquembre/clj-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };
  outputs = { self, nixpkgs, flake-utils, clj-nix }:

    flake-utils.lib.eachDefaultSystem
      (system:
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
              buildInputs = [ pkgs.babashka pkgs.unzip ];

              jdkRunner = pkgs.jdk17_headless;
              buildCommand =
                ''
                  bb cli-prod-jar
                  export jarPath=clojure-lsp-standalone.jar
                '';
              doCheck = true;
              checkPhase = "bb test";
            };

            clojure-lsp = cljpkgs.mkGraalBin {
              cljDrv = self.packages."${system}".clojure-lsp-jdk;
            };

          };
          devShells.default =
            let
              deps-lock-update = pkgs.writeShellApplication {
                name = "deps-lock-update";
                runtimeInputs = [ cljpkgs.deps-lock ];
                text = "deps-lock --bb --alias-exclude debug";
              };
            in
            pkgs.mkShell
              {
                packages = [ deps-lock-update ];
              };
        }) // {
      overlays.default = (final: prev: {
        clojure-lsp = self.packages.${final.system}.default;
      });
    };
}

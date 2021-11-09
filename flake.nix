{
  description = "Clojure-lsp Flake";
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }: {
    defaultPackage.x86_64-linux =
      let pkgs = import nixpkgs { system = "x86_64-linux"; };
      in pkgs.callPackage ./default.nix { };

    defaultPackage.aarch64-linux =
      let pkgs = import nixpkgs { system = "aarch64-linux"; };
      in pkgs.callPackage ./default.nix { };

    defaultPackage.x86_64-darwin =
      let pkgs = import nixpkgs { system = "x86_64-darwin"; };
      in pkgs.callPackage ./default.nix { };

    defaultPackage.aarch64-darwin = # temporary hack
      let pkgs = import nixpkgs { system = "x86_64-darwin"; };
      in pkgs.callPackage ./default.nix { };
  };
}

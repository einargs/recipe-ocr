{
  description = "A very basic flake";
  inputs.haskellNix.url = "github:input-output-hk/haskell.nix";
  inputs.nixpkgs.follows = "haskellNix/nixpkgs-unstable";
  inputs.flake-utils.url = "github:numtide/flake-utils";
  outputs = { self, nixpkgs, flake-utils, haskellNix }:
    flake-utils.lib.eachSystem [ "x86_64-linux" "x86_64-darwin" ] (system:
    let
      overlays = [ haskellNix.overlay
        (final: prev: {
          # This overlay adds our project to pkgs
          recipeOcrServer =
            final.haskell-nix.project' {
              src = ../.;
              compiler-nix-name = "ghc902";
              # This is used by `nix develop .` to open a shell for use with
              # `cabal`, `hlint` and `haskell-language-server`
              # shell.tools = {
              #   cabal = {};
              #   hlint = {};
              #   haskell-language-server = {};
              # };
              # # Non-Haskell shell tools go here
              # shell.buildInputs = with pkgs; [
              #   nixpkgs-fmt
              # ];
              # This adds `js-unknown-ghcjs-cabal` to the shell.
              # shell.crossPlatforms = p: [p.ghcjs];
            };
        })
      ];
      pkgs = import nixpkgs { inherit system overlays; inherit (haskellNix) config; };
      flake = pkgs.pkgsCross.raspberryPi.recipeOcrServer.flake {
        # This adds support for `nix build .#js-unknown-ghcjs:hello:exe:hello`
        # crossPlatforms = p: [p.ghcjs];
      };
      rawPackage = flake.packages."recipe-ocr:exe:recipe-ocr-exe";
      patched = pkgs.runCommand "recipe-ocr-patched" { } ''
        set -eu
        ls ${rawPackage}/bin/
        cp ${rawPackage}/bin/recipe-ocr $out
        chmod +w $out
        ${pkgs.patchelf}/bin/patchelf --set-interpreter /lib/ld-linux-aarch64.so.1 --set-rpath /lib:/usr/lib $out
        chmod -w $out
      '';
    in flake // {
      # Built by `nix build .`
      packages.default = patched;
    });
}

{
  description = "server for recipe OCR and organization";
  inputs = {
    nixpkgs.url = "nixpkgs/nixos-22.11";
    flake-utils.url = "github:numtide/flake-utils";
  };
  outputs = { self, nixpkgs, flake-utils }:
  flake-utils.lib.eachSystem [ "x86_64-linux" ] (system:
    with import nixpkgs { system = system; };
    let
      raw-pi = pkgsCross.aarch64-multiplatform.haskell.packages
        .ghc925.callPackage ./server-package.nix { };
      patched-pi = runCommand "recipe-ocr-patched" { } ''
        set -eu
        cp ${raw-pi}/bin/recipe-ocr $out
        chmod +w $out
        ${native.patchelf}/bin/patchelf --set-interpreter /lib/ld-linux-aarch64.so.1 --set-rpath /lib:/usr/lib $out
        chmod -w $out
      '';
    in {
      nixConfig = ''
        bash-prompt = \[\033[1;32m\][\[\e]0;\u@\h: \w\a\]dev-shell:\w]\$\[\033[0m\] 
      '';
      devShells.default = mkShell {
        buildInputs = [ stack haskell.compiler.ghc925 sqlite
          tesseract4 nodejs-16_x cabal2nix ]
          ++ (with haskellPackages; [
            ghcid
            # Required by spacemacs haskell layer
            apply-refact hlint stylish-haskell hasktags hoogle
          ]);
        PINNED_NIX_PATH = nixpkgs;
        NIX_PATH="nixpkgs=${nixpkgs}";
      };
      packages.pi-build = patched-pi;
    }
  );

}

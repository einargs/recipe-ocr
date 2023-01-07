{
  description = "server for recipe OCR and organization";
  inputs = {
    nixpkgs.url = "nixpkgs/nixos-22.11";
    flake-utils.url = "github:numtide/flake-utils";
  };
  nixConfig = {
    bash-prompt = ''\[\033[1;32m\][\[\e]0;\u@\h: \w\a\]dev-shell:\w]\$\[\033[0m\] '';
  };
  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachSystem [ "x86_64-linux" "aarch64-linux" ] (system:
      with import nixpkgs { system = system; };
      let raw-pi = haskell.packages.ghc925.callPackage ./server-package.nix { };
          patched-pi = runCommand "recipe-ocr-patched" { } ''
            set -eu
            ls ${raw-pi}/bin/recipe-ocr-exe
            cp ${raw-pi}/bin/recipe-ocr-exe $out
            chmod +w $out
            ${patchelf}/bin/patchelf --set-interpreter /lib/ld-linux-aarch64.so.1 --set-rpath /lib:/usr/lib:/usr/local/lib $out
            chmod -w $out
          '';
      in {
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
        packages.default = raw-pi;
        packages.pi-build = patched-pi;
      }
    );

}

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
      with nixpkgs.legacyPackages.${system};
      let
        compiler = "ghc902";
        hPkgs = haskell.packages.${compiler};
        recipe-ocr = hPkgs.callPackage ./server-package.nix { };
        stack-wrapped = symlinkJoin {
          name = "stack";
          paths = [ stack ];
          buildInputs = [ makeWrapper ];
          postBuild = ''
            wrapProgram $out/bin/stack --add-flags \
              "--no-nix --system-ghc --no-install-ghc"
          '';
        };
      in {
        devShells.default = mkShell {
          buildInputs = [ stack haskell.compiler.${compiler} sqlite
            tesseract4 nodejs-16_x cabal2nix ]
            ++ (with hPkgs; [
              ghcid
              # Required by spacemacs haskell layer
              apply-refact hlint stylish-haskell hasktags hoogle
            ]);
          # Hack to make c stuff available to GHC
          # see: https://docs.haskellstack.org/en/stable/nix_integration/
          LD_LIBRARY_PATH = pkgs.lib.makeLibraryPath [ zlib ];
        };

        packages.default = recipe-ocr;
      }
    );
}

{
  description = "server for recipe OCR and organization";
  inputs = {
    nixpkgs.url = "nixpkgs/nixos-22.11";
    flake-utils.url = "github:numtide/flake-utils";
    static-haskell-nix = {
      url = "github:nh2/static-haskell-nix/master";
      flake = false;
    };
  };
  nixConfig = {
    bash-prompt = ''\[\033[1;32m\][\[\e]0;\u@\h: \w\a\]dev-shell:\w]\$\[\033[0m\] '';
  };
  outputs = { self, nixpkgs, static-haskell-nix, flake-utils }:
    flake-utils.lib.eachSystem [ "x86_64-linux" "aarch64-linux" ] (system:
      with import nixpkgs { system = system; };
      let
        compiler = "ghc925";
        cabalPackageName = "recipe-ocr-exe";
        stack2nix-output-path = "stack2nix-output.nix";
        stack2nix-script = import "${static-haskell-nix}/static-stack2nix-builder/stack2nix-script.nix" {
          inherit compiler pkgs;
          stack-project-dir = ./.;
          # only for pinning extra-deps; we only have to worry about command.
          hackageSnapshot = "2021-07-11T00:00:00Z";
        };

        static-stack2nix-builder = import "${static-haskell-nix}/static-stack2nix-builder/default.nix" {
          normalPkgs = pkgs;
          inherit cabalPackageName compiler stack2nix-output-path;
          # disableOptimization = true; # for compile speed
        };
        static-pi = static-stack2nix-builder.static_package;
        # raw-pi = pkgsStatic.haskell.packages.${compiler}.callPackage ./server-package.nix { };
        # patched-pi = runCommand "recipe-ocr-patched" { } ''
        #   set -eu
        #   ls ${raw-pi}/bin/recipe-ocr-exe
        #   cp ${raw-pi}/bin/recipe-ocr-exe $out
        #   chmod +w $out
        #   ${patchelf}/bin/patchelf --set-interpreter /lib/ld-linux-aarch64.so.1 --set-rpath /lib:/usr/lib:/usr/local/lib $out
        #   chmod -w $out
        # '';
      in {
        devShells.default = mkShell {
          buildInputs = [ stack haskell.compiler.${compiler} sqlite
            tesseract4 nodejs-16_x cabal2nix ]
            ++ (with haskellPackages; [
              ghcid
              # Required by spacemacs haskell layer
              apply-refact hlint stylish-haskell hasktags hoogle
            ]);
          PINNED_NIX_PATH = nixpkgs;
          NIX_PATH="nixpkgs=${nixpkgs}";
        };
        # packages.default = raw-pi;
        # packages.pi-build = patched-pi;
        packages.static-pi = static-pi;
      }
    );

}

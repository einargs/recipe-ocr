{
  description = "server for recipe OCR and organization";
  inputs = {
    nixpkgs.url = "nixpkgs/nixos-22.11";
    flake-utils.url = "github:numtide/flake-utils";
    deploy-rs.url = "github:serokell/deploy-rs";
  };
  nixConfig = {
    bash-prompt = ''\[\033[1;32m\][\[\e]0;\u@\h: \w\a\]dev-shell:\w]\$\[\033[0m\] '';
  };
  outputs = { self, nixpkgs, deploy-rs, flake-utils }:
  let
    per-system =
      flake-utils.lib.eachSystem [ "x86_64-linux" "aarch64-linux" ] (system:
        with nixpkgs.legacyPackages.${system};
        let
          compiler = "ghc902";
          nodejs = nodejs-18_x;
          hPkgs = haskell.packages.${compiler};
          recipe-ocr-src = nixpkgs.lib.sourceByRegex ./. [
            "^Setup.hs$"
            "^src/?.*"
            "^app/?.*"
            "^test/?.*"
            "^stack.yaml$"
            "^stack.yaml.lock$"
            "^package.yaml$"
          ];
          recipe-ocr = hPkgs.callCabal2nix "recipe-ocr" recipe-ocr-src { };
          stack-wrapped = symlinkJoin {
            name = "stack";
            paths = [ stack ];
            buildInputs = [ makeWrapper ];
            postBuild = ''
              wrapProgram $out/bin/stack --add-flags \
                "--no-nix --system-ghc --no-install-ghc"
            '';
          };
          deploy = deploy-rs.defaultPackage.${system};
          site-dist = import ./recipe-site/site-dist.nix {
            inherit pkgs system nodejs;
          };
        in {
          devShells.default = mkShell {
            buildInputs = [ stack-wrapped haskell.compiler.${compiler} sqlite
              tesseract4 nodejs cabal2nix zlib deploy node2nix ]
                ++ (with hPkgs; [
                  ghc ghcid
                  # Required by spacemacs haskell layer
                  apply-refact hlint stylish-haskell hasktags hoogle
                ]);
            # Hack to make c stuff available to GHC
            # see: https://docs.haskellstack.org/en/stable/nix_integration/
            LD_LIBRARY_PATH = pkgs.lib.makeLibraryPath [ zlib hPkgs.ghc ];
            LOCAL_KEY = "/var/cache-priv-key.pem";
          };

          packages.default = recipe-ocr;
          packages.server-app = recipe-ocr;
          packages.site-dist = site-dist;
        });
    in per-system // {
      nixosConfigurations.recipes = nixpkgs.lib.nixosSystem {
        system = "aarch64-linux";
        specialArgs = {
          recipe-ocr = self.packages.aarch64-linux.server-app;
          recipe-site = self.packages.aarch64-linux.site-dist;
        };
        modules = [
          # Additional NixOS modules, like Home Manager or personal modules
          "${nixpkgs}/nixos/modules/installer/sd-card/sd-image-aarch64.nix"
          # Try headless later, not needed right away
          # "${nixpkgs}/nixos/modules/profiles/headless.nix"
          ./pi-config/config.nix
        ];
      };

      deploy.nodes.recipes = {
        # Fuck Avahi. It is still unreliable sometimes, so we'll use the
        # builtin router mdns.
        hostname = "recipes.attlocal.net";
        # hostname = "recipes.local";
        profiles.system = {
          magicRollback = true; # may need to disable
          sshUser = "pi";
          # sshOpts = [ "-t" ];
          # NOTE: if you need to pass the password to sudo but the tty isn't
          # working, you can also do `sudo = "echo password | sudo -S -u";`.
          # -S makes sudo read the password from stdin.
          user = "root";
          path = deploy-rs.lib.aarch64-linux.activate.nixos
            self.nixosConfigurations.recipes;
        };
      };
      checks = builtins.mapAttrs
        (system: deployLib: deployLib.deployChecks self.deploy)
        deploy-rs.lib;
    };
}

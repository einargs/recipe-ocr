let pinned-nixpkgs-path = import ./pinned-nixpkgs.nix;
    pinned-pkgs = import pinned-nixpkgs-path {};
in { nixpkgs ? pinned-pkgs }:
let 
  native = nixpkgs.pkgs;
  app = nixpkgs.pkgs.pkgsCross.raspberryPi.haskell.packages
            .ghc901.callPackage ./server-package.nix { };
in native.runCommand "recipe-ocr-patched" { } ''
      set -eu
      cp ${app}/bin/recipe-ocr $out
      chmod +w $out
      ${native.patchelf}/bin/patchelf --set-interpreter /lib/ld-linux-aarch64.so.1 --set-rpath /lib:/usr/lib $out
      chmod -w $out
    ''

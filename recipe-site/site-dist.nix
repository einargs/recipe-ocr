{pkgs, system, nodejs}:

# To update the node packages run
# `node2nix --development -l package-lock.json`
let nodeDependencies = (pkgs.callPackage ./default.nix {
    inherit system nodejs pkgs;
  }).nodeDependencies;

in pkgs.stdenv.mkDerivation {
  name = "recipe-site-dist";
  src = ./.;
  buildInputs = [ nodejs ];
  buildPhase = ''
    ln -s ${nodeDependencies}/lib/node_modules ./node_modules
    export PATH="${nodeDependencies}/bin:$PATH"
    npm run build
  '';
  installPhase = ''
    cp -r ./dist $out/
  '';
  # We're distributing client source files so there's nothing to patch.
  dontFixup = true;
}

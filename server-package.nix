{ mkDerivation, aeson, base, bytestring, containers, esqueleto
, filepath, ilist, lib, monad-logger, mtl, optparse-applicative
, persistent, persistent-sqlite, persistent-template, process
, resource-pool, servant, servant-multipart, servant-server
, temporary, text, transformers, wai, wai-app-static, wai-cors
, wai-extra, warp
}:
mkDerivation {
  pname = "recipe-ocr";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base bytestring containers esqueleto filepath ilist
    monad-logger mtl optparse-applicative persistent persistent-sqlite
    persistent-template process resource-pool servant servant-multipart
    servant-server temporary text transformers wai wai-app-static
    wai-cors wai-extra warp
  ];
  executableHaskellDepends = [
    aeson base bytestring containers esqueleto filepath ilist
    monad-logger mtl optparse-applicative persistent persistent-sqlite
    persistent-template process resource-pool servant servant-multipart
    servant-server temporary text transformers wai wai-app-static
    wai-cors wai-extra warp
  ];
  testHaskellDepends = [
    aeson base bytestring containers esqueleto filepath ilist
    monad-logger mtl optparse-applicative persistent persistent-sqlite
    persistent-template process resource-pool servant servant-multipart
    servant-server temporary text transformers wai wai-app-static
    wai-cors wai-extra warp
  ];
  homepage = "https://github.com/githubuser/recipe-ocr#readme";
  license = lib.licenses.bsd3;
  mainProgram = "recipe-ocr-exe";
}

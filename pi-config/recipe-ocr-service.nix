{config, pkgs, lib, recipe-ocr, recipe-site, ... }:

let
  cfg = config.services.recipe-ocr-server;
in

with lib;

{
  options = {
    services.recipe-ocr-server = {
      enable = mkEnableOption "recipe-ocr-server";

      port = mkOption {
        type = with types; nullOr port;
        default = 80;
        example = 8000;
        description = "The port to bind the server to.";
      };
    };
  };

  config = mkIf cfg.enable {
    systemd.services.recipe-ocr = {
      # TODO: figure out how to setup the proper user. See TODO.md for info.
      description = "recipe-ocr server";
      wantedBy = [ "multi-user.target" ];
      after = [ "network.target" ];
      path = with pkgs; [ tesseract4 ];
      serviceConfig = {
        StateDirectory = "recipe-ocr";
        ExecStart = "${recipe-ocr}/bin/recipe-ocr-exe"
          + " --port ${builtins.toString cfg.port}"
          + " --site ${recipe-site}"
          + " --db /var/lib/recipe-ocr/recipes.db";
      };
    };
  };
}

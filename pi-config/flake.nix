{
  description = "NixOS Raspberry Pi configuration flake";
  inputs = {
    recipe-ocr.url = "path:./..";
    nixpkgs.follows = "recipe-ocr/nixpkgs";
    deploy-rs.url = "github:serokell/deploy-rs";
  };

  outputs = { self, nixpkgs, deploy-rs, recipe-ocr }:
    let recipe-server = recipe-ocr.packages.aarch64-linux.default;
    in {
    nixosConfigurations.recipe-pi = nixpkgs.lib.nixosSystem {
      system = "aarch64-linux";
      modules = [
        # Additional NixOS modules, like Home Manager or personal modules
        "${nixpkgs}/nixos/modules/installer/sd-card/sd-image-aarch64.nix"
        # Try headless later, not needed right away
        # "${nixpkgs}/nixos/modules/profiles/headless.nix"

        # Inline configuration here
        ({ pkgs, ... }: {
          config = {
            # ...like <hostname>
            networking = {
              hostName = "recipe-pi";
              domain = "local";
              useDHCP = true;
              firewall.allowedTCPPorts = [ 22 80 443 8000 ];
              wireless = {
                enable = true;
                networks = {
                  Raettig.pskRaw = "fa56f80b59a80a4461dc93c9a1fd5b1d76d59180d7524494e0b20351d9fd72fd";
                };
              };
            };
            system.stateVersion = "22.11";
            services.openssh = {
              enable = true;
            };
            services.avahi = {
              enable = true;
              nssmdns = true;
              publish = {
                enable = true;
                domain = true;
                addresses = true;
              };
            };
            users.mutableUsers = false;
            users.users.pi = {
                isNormalUser = true;
                home = "/home/pi";
                description = "main user";
                extraGroups = [
                  "wheel" # allow sudo
                  "disk" "audio" "video" "networkmanager" "systemd-journal"
                ];
                hashedPassword = "$y$j9T$LWk0sZbX9Prm.XTQy36rU0$B/YFm6fcrjRtlTR1tOC6plDTTWNKxCCnQjvOX719Ii8";
                openssh.authorizedKeys.keys = [
                  "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAICpiOOnaTALPgrSRXAmY/49QN3CGAIwCmlFm9UjBsYWi einargs@gmail.com"
                ];
            };
            environment.systemPackages = with pkgs; [
              tesseract4 recipe-server sqlite ];
            # Go over systemd stuff later
            # systemd.services.recipe-ocr = {
            #   description = "recipe-ocr server";
            #   wantedBy = [ "multi-user.target" ];
            #   after = [ "network.target" ];
            #   serviceConfig = {
            #     ExecStart = ''${recipe-server}/bin/recipe-ocr-exe'';
            #   };
            # };
          };
        })
      ];
    };
    deploy.nodes.recipe-pi = {
      hostname = "recipe-pi.local";
      profiles.system = {
        magicRollback = true; # may need to disable
        sshUser = "pi";
        user = "root";
        path = deploy-rs.lib.aarch64-linux.activate.nixos
          self.nixosConfigurations.recipe-pi;
      };
    };
    checks = builtins.mapAttrs
      (system: deployLib: deployLib.deployChecks self.deploy)
      deploy-rs.lib;
  };
}

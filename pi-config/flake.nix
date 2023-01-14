{
  description = "NixOS Raspberry Pi configuration flake";
  inputs = {
    recipe-ocr.url = "path:./..";
    nixpkgs.follows = "recipe-ocr/nixpkgs";
    deploy-rs.url = "github:serokell/deploy-rs";
  };

  outputs = { self, nixpkgs, deploy-rs, recipe-ocr }:
    let recipe-server = recipe-ocr.packages.aarch64-linux.default;
        pkgs = nixpkgs.legacyPackages.x86_64-linux;
    in {
    devShells.x86_64-linux.default = pkgs.mkShell {
      buildInputs = [ deploy-rs.defaultPackage.x86_64-linux ];
      LOCAL_KEY = "/var/cache-priv-key.pem";
    };

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
            nix.settings = {
              trusted-public-keys = [
                (builtins.readFile ./cache-pub-key.pem)
                "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="
              ];
            };
            system.stateVersion = "22.11";
            services.openssh = {
              enable = true;
            };
            services.avahi = {
              enable = true;
              nssmdns = true;
              ipv6 = false;
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
            security.sudo.wheelNeedsPassword = false;
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
      # Fuck Avahi. It is still unreliable sometimes, so we'll use the builtin
      # router mdns.
      hostname = "recipe-pi.attlocal.net";
      # hostname = "recipe-pi.local";
      profiles.system = {
        magicRollback = true; # may need to disable
        sshUser = "pi";
        # sshOpts = [ "-t" ];
        # NOTE: if you need to pass the password to sudo but the tty isn't
        # working, you can also do `sudo = "echo password | sudo -S -u";`.
        # -S makes sudo read the password from stdin.
        user = "root";
        path = deploy-rs.lib.aarch64-linux.activate.nixos
          self.nixosConfigurations.recipe-pi;
      };
    };
    # checks = builtins.mapAttrs
    #   (system: deployLib: deployLib.deployChecks self.deploy)
    #   deploy-rs.lib;
  };
}

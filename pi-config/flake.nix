{
  description = "NixOS Raspberry Pi configuration flake";
  inputs = {
    recipe-ocr.url = "path:..";
    nixpkgs.follows = "recipe-ocr/nixpkgs";
  };

  outputs = { self, nixpkgs, recipe-ocr }:
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
              firewall.allowedTCPPorts = [ 22 80 443 ];
            };
            system.stateVersion = "22.11";
            services.openssh = {
              enable = true;
            };
            users.mutableUsers = false;
            users.users.pi = {
                isNormalUser = true;
                home = "/home/pi";
                description = "main user";
                extraGroups = [
                  "wheel" # allow sudo
                ];
                hashedPassword = "$y$j9T$LWk0sZbX9Prm.XTQy36rU0$B/YFm6fcrjRtlTR1tOC6plDTTWNKxCCnQjvOX719Ii8";
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
  };
}

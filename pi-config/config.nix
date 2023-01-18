{ pkgs, recipe-ocr, recipe-site, ... }: {
  imports = [
    ./recipe-ocr-service.nix
  ];
  config = {
    # ...like <hostname>
    networking = {
      # More convienient name for others.
      hostName = "recipes";
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
      # Might manually use mdns4, see:
      # https://discourse.nixos.org/t/help-with-local-dns-resolution/20305/5
      # probably try resolved first though.
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
      tesseract4 recipe-ocr recipe-site sqlite ];

    # See ./recipe-ocr-service.nix
    services.recipe-ocr-server = {
      enable = true;
      port = 80;
    };
  };
}

{ config, pkgs, ... }:

{
  imports = [ ../../modules/ssh-keys.nix ];
  networking.hostName = "podman-server";

  time.timeZone = "America/Argentina/Cordoba";

  i18n.defaultLocale = "en_US.UTF-8";

  users.users.wmb = {
    isNormalUser = true;
    description = "wmb";
    extraGroups = [ "networkmanager" "wheel" "podman" ];
    packages = with pkgs; [];
  };

  services.getty.autologinUser = "wmb";

  environment.systemPackages = with pkgs; [
    git
    curl
    wget
    podman-compose
  ];

  virtualisation.podman = {
    enable = true;
    dockerCompat = true;  # `docker` command aliased to podman
    defaultNetwork.settings.dns_enabled = true;
  };

  # Proxmox integration
  services.qemuGuest.enable = true;

  services.openssh = {
    enable = true;
    settings = {
      PasswordAuthentication = false;
      PermitRootLogin = "no";
    };
  };

  networking.networkmanager.enable = true;

  networking.firewall = {
    enable = true;
    allowedTCPPorts = [ 22 3000 2222 8080 5000 8978];
  };

  nix.settings.require-sigs = false;
  nix.settings.trusted-users = [ "root" "wmb" ];

  security.sudo.wheelNeedsPassword = false;

  programs.gnupg.agent = {
    enable = true;
    enableSSHSupport = true;
  };

  system.stateVersion = "25.11";
}

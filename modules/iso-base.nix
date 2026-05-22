{ config, pkgs, lib, user, ... }:

{
  # Core system
  nix.settings.experimental-features = [ "nix-command" "flakes" ];

  networking.networkmanager.enable = true;

  time.timeZone = "America/Argentina/Buenos_Aires";

  i18n = {
    defaultLocale = "en_US.UTF-8";
    extraLocaleSettings = {
      LC_TIME = "es_AR.UTF-8";
      LC_MONETARY = "es_AR.UTF-8";
    };
  };

  # SSH (safe for ISO + servers)
  services.openssh.enable = true;

  # Live user — empty password so the greeter just needs username + enter
  users.users.${user} = {
    isNormalUser = true;
    extraGroups = [ "wheel" "networkmanager" ];
    password = "";
  };
  security.pam.services.login.allowNullPassword = true;

  # Common tools — ruby runs install.rb, disko and git are used during install
  environment.systemPackages = with pkgs; [
    ruby
    git
    tree
    gptfdisk
    (pkgs.writeScriptBin "install" ''
      #!/usr/bin/env sh
      exec ruby /etc/nixos/setup/install.rb "$@"
    '')
  ];

  # Bundle the setup repo into the ISO
  environment.etc."nixos/setup".source = lib.cleanSource ../.;

  # Audio (modern default)
  services.pulseaudio.enable = false;
  security.rtkit.enable = true;

  services.pipewire = {
    enable = true;
    alsa.enable = true;
    pulse.enable = true;
  };

  # ISO-specific greeter overrides
  services.crystal-greeter = {
    title = "NixOS Installer  |  login: ${user}  |  password: (empty)";
    menu = [
      { action = "reboot";   }
      { action = "shutdown"; }
    ];
  };

  # DBus + desktop compatibility
  services.dbus.enable = true;
  programs.dconf.enable = true;

  system.stateVersion = "24.11";
}

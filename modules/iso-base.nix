{ config, pkgs, lib, ... }:

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

  # User (generic)
  users.users.wmb = {
    isNormalUser = true;
    extraGroups = [ "wheel" "networkmanager" ];
    hashedPassword = "$6$PhrjZtGlKDk6ZEEf$K5K0xt1A1onZZCfIpLHd4Dd6zldhoC7UL4Y0jCzlEkAT/ssqtFskd/RYhH.2W9HDhvOM8BSnQkik3w9pNjqjO0";
  };

  # Common tools
  environment.systemPackages = with pkgs; [
    git
    tree
  ];

  # Audio (modern default)
  hardware.pulseaudio.enable = false;
  security.rtkit.enable = true;

  services.pipewire = {
    enable = true;
    alsa.enable = true;
    pulse.enable = true;
  };

  # DBus + desktop compatibility
  services.dbus.enable = true;
  programs.dconf.enable = true;

  system.stateVersion = "24.11";
}

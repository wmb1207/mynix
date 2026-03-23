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
    hashedPassword = "$y$j9T$jp/HFYKwvCEghPwTx5VTL/$1BDWi3DMJjgPub.Xl6LlouxDhhFleiikrI.bYl./uq1";
  };

  # Common tools
  environment.systemPackages = with pkgs; [
    git
    tree
    gptfdisk
    (pkgs.writeScriptBin "install-wmb" (builtins.readFile ../install.sh))
  ];

  # Copy flake to /etc/nixos/setup on ISO
  environment.etc."nixos/setup".source = lib.cleanSource ../.;

  # Audio (modern default)
  services.pulseaudio.enable = false;
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

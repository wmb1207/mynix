{ config, pkgs, lib, ... }:
{
  nix.settings.experimental-features = [ "nix-command" "flakes" ];
  nix.enable = true;

  # Allow unfree packages
  nixpkgs.config.allowUnfree = true;

  # Shell — must be declared for nix-darwin to accept it as a login shell
  programs.zsh.enable = true;
  environment.shells = [ pkgs.oksh ];

  system.primaryUser = "wmb";

  # macOS system defaults
  system.defaults = {
    dock.autohide = true;
    dock.show-recents = false;
    dock.minimize-to-application = true;

    finder.AppleShowAllExtensions = true;
    finder.ShowPathbar = true;
    finder.FXPreferredViewStyle = "Nlsv"; # list view

    NSGlobalDomain.KeyRepeat = 2;
    NSGlobalDomain.InitialKeyRepeat = 15;
    NSGlobalDomain."com.apple.swipescrolldirection" = false;
    NSGlobalDomain.AppleInterfaceStyle = "Dark";
  };

  # Keyboard — Caps Lock → Control (Dvorak must be set manually in System Settings once)
  system.keyboard.enableKeyMapping = true;
  system.keyboard.remapCapsLockToControl = true;

  # Homebrew for GUI apps not in nixpkgs or better as casks
  homebrew = {
    enable = true;
    onActivation.cleanup = "zap";
    onActivation.autoUpdate = true;
    casks = [
      "ghostty"
      "firefox"
      "brave-browser"
      "vlc"
      "karabiner-elements" # Alt/Ctrl swap (swap_lalt_lctl equivalent)
      "raycast"            # dmenu equivalent (set cmd+space in Raycast prefs)
    ];
  };

  # SSH
  services.openssh.enable = true;
  programs.ssh.startAgent = true;

  system.stateVersion = 5;
}

{ lib }:

let
  mkSource = path: { source = path; };
  
  mkExecutable = path: { source = path; executable = true; };

  wallpapers = max: builtins.listToAttrs (map (x: {
    name = ".config/wallpapers/wallpaper-${toString x}.jpg";
    value.source = ../assets + "/wallpaper-${toString x}.jpg";
  }) (lib.range 1 max));

in {
  inherit mkSource mkExecutable wallpapers;

  assets = {
    ".xinitrc" = mkSource ../assets/xinitrc;
    ".bashrc" = lib.mkForce (mkSource ../assets/bashrc);

    # configs
    ".config/ghostty/config" = mkSource ../assets/ghostty;
    ".config/polybar/config.ini" = mkSource ../assets/polybar.ini;
    ".config/polybar/launch.sh" = mkExecutable ../assets/polybar-start.sh;
    ".config/picom/picom.conf" = mkSource ../assets/picom.conf;
    ".config/wallpapers/gradient.png" = mkSource ../assets/gradient.png;
    ".config/wallpapers/galaxy-plant.jpeg" = mkSource ../assets/galaxy-plant.jpeg;
    ".config/dunst/dunstrc" = mkSource ../assets/dunstrc;
    ".config/libinput-gestures.conf" = mkSource ../assets/libinput-gestures.conf;
    ".fvwm/config" = mkSource ../assets/fvwm3.conf;
    ".emacs.d/init.el" = mkSource ../assets/init.el;
    ".emacs.d/lisp/packages.el" = mkSource ../assets/packages.el;

    #scripts
    ".local/bin/battery.rb"   = mkExecutable ../assets/scripts/battery.rb;
    ".local/bin/lock.sh"      = mkExecutable ../assets/scripts/lock.sh;
    ".local/bin/dock.rb"      = mkExecutable ../assets/scripts/dock.rb;
    ".local/bin/backlight.rb" = mkExecutable ../assets/scripts/backlight.rb;
    ".local/bin/polybar.sh"   = mkExecutable ../assets/scripts/polybar.sh;
    ".local/bin/keys.rb"      = mkExecutable ../assets/scripts/keys.rb;
    ".local/bin/tunnel.rb"    = mkExecutable ../assets/scripts/tunnel.rb;
    ".local/bin/projecter.rb" = mkExecutable ../assets/scripts/projecter.rb;
    ".local/bin/theme.rb"        = mkExecutable ../assets/scripts/theme.rb;
    ".local/bin/kbd.rb"          = mkExecutable ../assets/scripts/kbd.rb;
    ".local/bin/screen_setup.rb" = mkExecutable ../assets/scripts/screen_setup.rb;
    ".local/bin/dmenu.sh"        = mkExecutable ../assets/scripts/dmenu.sh;

    # templates - dev-shells
    # ".local/templates/php.nix" = mkSource ../templates/devshells/php.nix;
    # ".local/templates/python.nix" = mkSource ../templates/devshells/python.nix;
    # ".local/templates/nodejs.nix" = mkSource ../templates/devshells/nodejs.nix;
    # ".local/templates/raw.nix" = mkSource ../templates/devshells/raw.nix;
  };
}
    

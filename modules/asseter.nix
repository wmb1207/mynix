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
    ".config/bspwm/bspwmrc" = mkExecutable ../assets/bspwmrc;
    ".config/sxhkd/sxhkdrc" = mkSource ../assets/sxhkdrc;
    ".config/polybar/config.ini" = mkSource ../assets/polybar.ini;
    ".config/polybar/launch.sh" = mkExecutable ../assets/polybar-start.sh;
    ".config/picom/picom.conf" = mkSource ../assets/picom.conf;
    ".config/wallpapers/gradient.png" = mkSource ../assets/gradient.png;
    ".config/wallpapers/galaxy-plant.jpeg" = mkSource ../assets/galaxy-plant.jpeg;
    ".config/dunst/dunstrc" = mkSource ../assets/dunstrc;
    ".fvwm/config" = mkSource ../assets/fvwm3.conf;

    #scripts
    ".local/bin/battery.clj" = mkExecutable ../assets/scripts/battery.clj;
    ".local/bin/lock.sh" = mkExecutable ../assets/scripts/lock.sh;
    ".local/bin/dock.clj" = mkExecutable ../assets/scripts/dock.clj;
    ".local/bin/backlight.clj" = mkExecutable ../assets/scripts/backlight.clj;
    ".local/bin/polybar.sh" = mkExecutable ../assets/scripts/polybar.sh;

    # templates - dev-shells
    # ".local/templates/php.nix" = mkSource ../templates/devshells/php.nix;
    # ".local/templates/python.nix" = mkSource ../templates/devshells/python.nix;
    # ".local/templates/nodejs.nix" = mkSource ../templates/devshells/nodejs.nix;
    # ".local/templates/raw.nix" = mkSource ../templates/devshells/raw.nix;
  };
}
    

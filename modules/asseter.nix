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
    ".config/ghostty/config".source = ../assets/ghostty;

    #scripts
    ".local/bin/battery.sh" = mkSource ../assets/scripts/battery.sh;
    ".local/bin/lock.sh" = mkSource ../assets/scripts/lock.sh;
    ".local/bin/dock.sh" = mkSource ../assets/scripts/dock.sh;
  };
}
    

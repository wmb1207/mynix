{ pkgs }:

with pkgs; [
  libnotify
  picom
  feh
  (dmenu.overrideAttrs (old: {
    patches = (old.patches or []) ++ [ ./dmenu-xyw.patch ./dmenu-border.patch ];
  }))
  polybar
  eww
  dunst
  pulsemixer
]

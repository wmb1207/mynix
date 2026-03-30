{ pkgs }:

with pkgs; [
  libnotify
  picom
  feh
  (dmenu.overrideAttrs (old: {
    patches = (old.patches or []) ++ [ ./dmenu-xyw.patch ];
  }))
  polybar
  eww
  dunst
  pulsemixer
]

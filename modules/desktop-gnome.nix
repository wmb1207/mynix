{ config, pkgs, ... }:

{
  services.xserver = {
    enable = true;
    # displayManager.gdm.enable = true;:
    # displayManager.defaultSession = "gnome";
    # desktopManager.gnome.enable = true;
  };
}

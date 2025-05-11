{ config, pkgs, ... }:

{
  # Configure xkb options to remap the keyboard as per the requirements
  services.xserver = {
    # Define the XKB options for remapping the keys
    xkbOptions = "ctrl:nocaps,altwin:meta,ctrl:swap_lalt_lctl";
  };

  # Apply the custom keymap via X11 configuration
  environment.etc."X11/xorg.conf.d/00-keyboard.conf".text = ''
    Section "InputClass"
      Identifier "keyboard"
      MatchIsKeyboard "on"
      Option "XkbLayout" "us"
      Option "XkbOptions" "ctrl:nocaps,altwin:meta,ctrl:swap_lalt_lctl"
    EndSection
  '';

  # Alternatively, remap specific keys using a custom script
  # that runs after the system boots, adding the key remaps
  systemd.user.services.keyboard-remap = {
    description = "Remap keys for laptop";
    serviceConfig.ExecStart = "${pkgs.xorg.xev}/bin/xev";
    wantedBy = [ "default.target" ];
  };
}

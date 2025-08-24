{ config, pkgs, ... }:

{
  # Apply the custom keymap via X11 configuration only for the built-in keyboard
  environment.etc."X11/xorg.conf.d/00-keyboard.conf".text = ''
    Section "InputClass"
      Identifier "internal-keyboard"
      MatchIsKeyboard "on"
      MatchProduct "AT Translated Set 2 keyboard"
      MatchDevicePath "/dev/input/event0"
      Option "XkbLayout" "us"
      Option "XkbVariant" "dvorak"
      Option "XkbOptions" "ctrl:nocaps,altwin:meta,ctrl:swap_lalt_lctl"
    EndSection
  '';

  # Optional: custom script (currently just xev — probably not needed for remapping)
  systemd.user.services.keyboard-remap = {
    description = "Remap keys for laptop";
    serviceConfig.ExecStart = "${pkgs.xorg.xev}/bin/xev";
    wantedBy = [ "default.target" ];
  };
}

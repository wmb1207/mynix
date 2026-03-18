{ pkgs, ... }:

{
  services.xserver.enable = true;
  services.xserver.libinput.enable = true;

  console = {
    keyMap = "dvorak";
  };

  programs.firefox.enable = true;

  fonts.fontDir.enable = true;
  

  environment.systemPackages = with pkgs; [
    glxinfo
    mangohud
  ];
}

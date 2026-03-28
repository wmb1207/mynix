{ pkgs, ... }:

{
  services.xserver.enable = true;
  services.libinput.enable = true;

  console = {
    keyMap = "dvorak";
    font = "Lat2-Terminus16";
    packages = with pkgs; [ terminus_font ];
  };

  programs.firefox.enable = true;

  fonts = {
    fontDir.enable = true;
    packages = with pkgs; [
      dejavu_fonts
      ibm-plex
      iosevka-bin
      dina-font
      jetbrains-mono
      terminus_font
      cozette
      tamzen
    ];
    fontconfig = {
      enable = true;
      defaultFonts = {
        monospace = [ "DejaVu Sans Mono" ];
        sansSerif = [ "DejaVu Sans" ];
        serif = [ "DejaVu Serif" ];
      };
    };
  };

  environment.systemPackages = with pkgs; [
    mangohud
  ];
}

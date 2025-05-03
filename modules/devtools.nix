{ pkgs, ... }:

{
  environment.systemPackages = with pkgs; [
    wget curl bash cmake gnumake git libvterm emacs gcc
  ];

  programs.mtr.enable = true;
  programs.gnupg.agent = {
    enable = true;
    enableSSHSupport = true;
  };
}

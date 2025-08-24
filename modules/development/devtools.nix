{ pkgs, ... }:

{
  environment.systemPackages = with pkgs; [
    wget curl bash cmake gnumake git libvterm emacs gcc
    bspwm sxhkd rsync docker-compose mesa vulkan-tools ngrok
  ];

  programs.mtr.enable = true;
  programs.gnupg.agent = {
    enable = true;
    enableSSHSupport = true;
  };

  services.openssh.enable = true;
}

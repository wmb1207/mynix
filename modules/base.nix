{ pkgs, lib, ... }:

{
  environment.systemPackages = with pkgs; [
    wget
    curl
    bash
    cmake
    gnumake
    git
    libvterm
    emacs
    gcc
    rsync
    docker-compose
    mesa
    vulkan-tools
    xsecurelock
    xautolock
    dockapps.wmsystemtray
    dockapps.wmsm-app
    dockapps.wmCalClock
    dockapps.libdockapp
    dockapps.AlsaMixer-app
    nfs-utils
  ];

  security.sudo.enable = false;
  security.doas = {
    enable = true;
    extraRules = [{
      groups = [ "wheel" ];
      keepEnv = true;
      persist = true;
    }];
  };

  programs.mtr.enable = true;
  programs.gnupg.agent = {
    enable = true;
    enableSSHSupport = true;
  };

  services.openssh.enable = true;

  services.xserver.enable = true;

  xdg.portal = {
    enable = true;
    extraPortals = [ pkgs.xdg-desktop-portal-gtk ];
    config.common.default = lib.mkDefault "gtk";
  };

  nix.settings.require-sigs = false;
}

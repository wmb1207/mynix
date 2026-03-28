{ pkgs, ... }:

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

  programs.mtr.enable = true;
  programs.gnupg.agent = {
    enable = true;
    enableSSHSupport = true;
  };

  services.openssh.enable = true;

  services.xserver.enable = true;
}

{ pkgs, system ? pkgs.system }:

let
  # Define groups of packages for each arch
  commonPackages = with pkgs; [
    brave
    maim
    slop
    xterm
    rxvt-unicode
    pavucontrol
    postman
    vlc
    mupdf
    librewolf
    arandr
    xfe
    edwood
    plan9port
    xclip
    xclock
    xload
    moonlight-qt
  ];

  x86_64Packages = with pkgs; [
    slack
    steam
    jetbrains.datagrip
  ];

  aarch64Packages = with pkgs; [
  ];

  # Compose the final package list based on system architecture
  archPackages = if builtins.match "x86_64.*" system != null then x86_64Packages
                 else if builtins.match "aarch64.*" system != null then aarch64Packages
                 else [];

in
commonPackages ++ archPackages

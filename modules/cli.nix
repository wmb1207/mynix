{ pkgs, isDarwin ? false }:

with pkgs;
let
  common = [
    # shell & core
    dtach
    direnv
    babashka

    # tools
    git
    curl
    wget
    fzf
    fd
    ripgrep
    jq
    yq
    unzip
    zip
    gnumake
    bat
    tmux
    lf
    htop
    gtop

    # data
    postgresql

    # media
    cmus
    mpv

    # dev
    libtool
    php84Packages.phpmd

    # nix
    nix-search-cli
    claude-code
  ];

  linux = [
    loksh
    abcde
    flac
    usbutils
    xdotool
    xdo
    docker-compose
    xclip
    dysk
    networkmanager
    libfido2
    pamixer
    ameba
    clipmenu
  ];

  darwin = [
    oksh
    colima
    docker
    docker-compose
  ];

in
  common ++ (if isDarwin then darwin else linux)

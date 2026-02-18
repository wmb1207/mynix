{ pkgs }:

with pkgs; [
  loksh
  dtach
  usbutils
  xdotool
  xdo
  docker-compose
  direnv
  babashka
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
  xclip

  dysk
  bat
  tmux

  lf
  htop
  gtop
  neofetch
  networkmanager

  postgresql
  libfido2

  cmus
  pamixer
  mpv

  libtool
  php84Packages.phpmd
  ameba # Crystal linter

  clipmenu
  nix-search-cli
  claude-code
]

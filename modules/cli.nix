{ pkgs }:

with pkgs; [
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
  teleport
  libfido2

  cmus
  pamixer
]

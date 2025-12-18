{ pkgs }:

with pkgs; [
  awscli2
  #terraform
  opentofu
  ssm-session-manager-plugin
]

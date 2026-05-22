# Placeholder used by the ISO installer before nixos-generate-config runs.
# install.rb overwrites this file in its temporary copy after partitioning.
{ modulesPath, lib, ... }:

{
  imports = [ (modulesPath + "/installer/scan/not-detected.nix") ];

  networking.useDHCP = lib.mkDefault true;
  nixpkgs.hostPlatform = lib.mkDefault "x86_64-linux";
}

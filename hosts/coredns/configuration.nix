{ config, pkgs, ... }:

{
  imports = [ ../../modules/ssh-keys.nix ];
  networking.hostName = "coredns";
  networking.networkmanager.enable = true;

  time.timeZone = "America/Argentina/Cordoba";

  i18n.defaultLocale = "en_US.UTF-8";

  i18n.extraLocaleSettings = {
    LC_ADDRESS = "es_AR.UTF-8";
    LC_IDENTIFICATION = "es_AR.UTF-8";
    LC_MEASUREMENT = "es_AR.UTF-8";
    LC_MONETARY = "es_AR.UTF-8";
    LC_NAME = "es_AR.UTF-8";
    LC_NUMERIC = "es_AR.UTF-8";
    LC_PAPER = "es_AR.UTF-8";
    LC_TELEPHONE = "es_AR.UTF-8";
    LC_TIME = "es_AR.UTF-8";
  };

  users.users.wmb = {
    isNormalUser = true;
    description = "wmb";
    extraGroups = [ "networkmanager" "wheel" ];
    packages = with pkgs; [];
  };

  services.getty.autologinUser = "wmb";

  environment.systemPackages = with pkgs; [ dig ];

  services.coredns = {
    enable = true;
    config = ''
      wmb.arpa {
        hosts {
          192.168.88.26 wmb.arpa
          192.168.88.2  proxmox.wmb.arpa
          192.168.88.26 nginx.wmb.arpa
          192.168.88.26 forgejo.wmb.arpa
          192.168.88.26 excalidraw.wmb.arpa
          192.168.88.26 cloudbeaver.wmb.arpa
          192.168.88.26 registry.wmb.arpa
          192.168.88.26 mailpit.wmb.arpa
          192.168.88.26 hoppscotch.wmb.arpa
          192.168.88.26 hoppscotch-api.wmb.arpa
          192.168.88.26 hoppscotch-admin.wmb.arpa
          192.168.88.18 nas.wmb.arpa
          192.168.88.27 coredns.wmb.arpa
          192.168.88.37 desktop.wmb.arpa
          fallthrough
        }
        log
        errors
      }

      . {
        forward . 181.30.140.135 181.30.140.196
        cache
        log
        errors
      }
    '';
  };

  networking.firewall = {
    enable = true;
    allowedTCPPorts = [ 22 53 ];
    allowedUDPPorts = [ 53 ];
  };

  services.openssh = {
    enable = true;
    settings = {
      PasswordAuthentication = false;
      PermitRootLogin = "no";
    };
  };

  security.sudo.wheelNeedsPassword = false;
  nix.settings.trusted-users = [ "root" "wmb" ];
  nix.settings.require-sigs = false;

  services.qemuGuest.enable = true;

  system.stateVersion = "25.11";
}

{ config, pkgs, ... }:

let
  # Generate with: htpasswd -nbB wmb <password>
  registryHtpasswd = "wmb:$2y$05$Wyczu.1kTe1OOUW4TGwyC.NMfS2vw0srPweCjpuJnbTtzgMW05T/G";
in
{
  imports = [ ../../modules/ssh-keys.nix ../../modules/wmb-arpa-ca.nix ];
  networking.hostName = "podman-server";

  time.timeZone = "America/Argentina/Cordoba";

  i18n.defaultLocale = "en_US.UTF-8";

  users.users.wmb = {
    isNormalUser = true;
    description = "wmb";
    extraGroups = [ "networkmanager" "wheel" "podman" ];
    packages = with pkgs; [];
  };

  services.getty.autologinUser = "wmb";

  environment.systemPackages = with pkgs; [
    git
    curl
    wget
  ];

  # ── Podman ────────────────────────────────────────────────────────────────
  virtualisation.podman = {
    enable = true;
    dockerCompat = true;
    defaultNetwork.settings.dns_enabled = true;
  };

  virtualisation.oci-containers = {
    backend = "podman";
    containers = {

      registry = {
        image = "docker.io/library/registry:2";
        user = "1000:1000";
        ports = [ "6000:5000" ];
        volumes = [
          "/data/registry/registry:/var/lib/registry"
          "/data/registry/auth:/auth"
        ];
        environment = {
          REGISTRY_AUTH = "htpasswd";
          REGISTRY_AUTH_HTPASSWD_REALM="Registry Realm";
          REGISTRY_AUTH_HTPASSWD_PATH="/auth/htpasswd";
        };
      };

      forgejo = {
        image = "codeberg.org/forgejo/forgejo:14-rootless";
        user = "1000:1000";
        environment = {
          USER_UID = "1000";
          USER_GID = "1000";
          FORGEJO__server__DOMAIN = "forgejo.wmb.arpa";
          FORGEJO__server__ROOT_URL = "https://forgejo.wmb.arpa/";
          FORGEJO__server__SSH_DOMAIN = "forgejo.wmb.arpa";
          FORGEJO__server__SSH_PORT = "2222";
          FORGEJO__server__START_SSH_SERVER = "true";
          FORGEJO__database__DB_TYPE = "sqlite3";
        };
        ports = [ "3000:3000" "2222:2222" ];
        volumes = [
          "/data/forgejo:/var/lib/gitea"
          "/etc/localtime:/etc/localtime:ro"
        ];
      };

      excalidraw = {
        image = "excalidraw/excalidraw:latest";
        ports = [ "5000:80" ];
      };

      cloudbeaver = {
        image = "dbeaver/cloudbeaver:latest";
        ports = [ "8978:8978" ];
        environment = {
          CB_SERVER_NAME = "CloudBeaver Server";
          CB_SERVER_URL = "http://192.168.88.38:8978";
          CB_ADMIN_NAME = "admin";
          CB_ADMIN_PASSWORD = "admin";
        };
        volumes = [
          "/data/cloudbeaver/workspace:/opt/cloudbeaver/workspace"
        ];
      };

    };
  };

  # ── Data disk ─────────────────────────────────────────────────────────────
  # Format /dev/sdb as ext4 on first boot if it has no filesystem.
  # After nixos-rebuild, reboot once — the disk gets labelled "data" and
  # all subsequent boots mount it automatically.
  # Adjust /dev/sdb if the iSCSI disk appears under a different name
  # (check with: lsblk  or  ls /dev/disk/by-path/).
  systemd.services.format-data-disk = {
    description = "Format iSCSI data disk on first boot";
    wantedBy = [ "data.mount" ];
    before    = [ "data.mount" ];
    unitConfig = {
      ConditionPathExists  = "/dev/sdb";
      DefaultDependencies  = false;
    };
    serviceConfig = {
      Type            = "oneshot";
      RemainAfterExit = true;
      ExecStart = pkgs.writeShellScript "format-data-disk" ''
        if ! ${pkgs.util-linux}/bin/blkid /dev/sdb >/dev/null 2>&1; then
          echo "No filesystem found on /dev/sdb — formatting as ext4..."
          ${pkgs.e2fsprogs}/bin/mkfs.ext4 -L data /dev/sdb
        fi
      '';
    };
  };

  fileSystems."/data" = {
    device  = "/dev/disk/by-label/data";
    fsType  = "ext4";
    options = [ "defaults" "nofail" ];
  };

  # Containers that use /data must wait for the mount, and create their
  # directories before starting (tmpfiles can't be relied on for this since
  # it may run before the mount completes).
  systemd.services.podman-registry = {
    requires = [ "data.mount" ];
    after    = [ "data.mount" ];
    serviceConfig.ExecStartPre = [
      "${pkgs.coreutils}/bin/mkdir -p /data/registry/registry"
      "${pkgs.coreutils}/bin/mkdir -p /data/registry/auth"
      (pkgs.writeShellScript "init-registry-htpasswd" ''
        if [ ! -f /data/registry/auth/htpasswd ]; then
          printf '%s\n' '${registryHtpasswd}' > /data/registry/auth/htpasswd
        fi
      '')
    ];
  };

  systemd.services.podman-forgejo = {
    requires = [ "data.mount" ];
    after    = [ "data.mount" ];
    serviceConfig.ExecStartPre = [
      "${pkgs.coreutils}/bin/mkdir -p /data/forgejo"
      "+${pkgs.coreutils}/bin/chown 1000:1000 /data/forgejo"
    ];
  };

  systemd.services.podman-cloudbeaver = {
    requires = [ "data.mount" ];
    after    = [ "data.mount" ];
    serviceConfig.ExecStartPre =
      "${pkgs.coreutils}/bin/mkdir -p /data/cloudbeaver/workspace";
  };

  # ── Network ───────────────────────────────────────────────────────────────
  networking.networkmanager.enable = true;

  networking.firewall = {
    enable = true;
    allowedTCPPorts = [ 22 3000 2222 5000 6000 8978 ];
  };

  # ── SSH ───────────────────────────────────────────────────────────────────
  services.openssh = {
    enable = true;
    settings = {
      PasswordAuthentication = false;
      PermitRootLogin = "no";
    };
  };

  # ── Proxmox integration ───────────────────────────────────────────────────
  services.qemuGuest.enable = true;

  nix.settings.require-sigs   = false;
  nix.settings.trusted-users  = [ "root" "wmb" ];

  security.sudo.wheelNeedsPassword = false;

  programs.gnupg.agent = {
    enable         = true;
    enableSSHSupport = true;
  };

  system.stateVersion = "25.11";
}

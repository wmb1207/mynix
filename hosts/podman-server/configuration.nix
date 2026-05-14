{ config, lib, pkgs, hoppscotchEnv, ... }:

let
  # Generate with: htpasswd -nbB wmb <password>
  registryHtpasswd = "wmb:$2y$05$Wyczu.1kTe1OOUW4TGwyC.NMfS2vw0srPweCjpuJnbTtzgMW05T/G";

  # ── Forgejo custom theme ───────────────────────────────────────────────────
  # Palette: matches the wmb.arpa index page / terminal aesthetic
  #   bg         #1c1a18  card       #242220  hover      #2c2a28
  #   border     #3a3830  text       #b5b2a0  muted      #6a6858
  #   teal/link  #4a6a78  olive      #657050
  forgejoHeaderTmpl = pkgs.writeText "forgejo-header.tmpl" ''
    <style>
    html, :root {
      --color-body:              #1c1a18;
      --color-box-body:          #242220;
      --color-box-header:        #1e1c1a;
      --color-primary:           #4a6a78;
      --color-primary-dark-1:    #3d5d6a;
      --color-primary-dark-2:    #2f4f5c;
      --color-primary-light-1:   #5a7a88;
      --color-primary-light-2:   #6a8a98;
      --color-primary-alpha-50:  rgba(74,106,120,0.5);
      --color-secondary:         #657050;
      --color-secondary-dark-1:  #566040;
      --color-secondary-light-1: #758060;
      --color-text:              #b5b2a0;
      --color-text-dark:         #d0cdb8;
      --color-text-light-1:      #8a877a;
      --color-text-light-2:      #6a6858;
      --color-text-light-3:      #4a4840;
      --color-border:            #3a3830;
      --color-border-dark:       #4a4840;
      --color-secondary-border:  #3a3830;
      --color-input-background:  #242220;
      --color-input-border:      #3a3830;
      --color-input-border-focus:#4a6a78;
      --color-nav-bg:            #1c1a18;
      --color-header-wrapper:    #1c1a18;
      --color-menu-bg:           #242220;
      --color-item-hover:        #2c2a28;
      --color-item-active:       #2c2a28;
      --color-sidebar-bg:        #1e1c1a;
      --color-code-bg:           #242220;
      --color-code-border:       #3a3830;
      --color-diff-removed-bg:   rgba(100,40,40,0.3);
      --color-diff-added-bg:     rgba(40,80,40,0.3);
      --color-shadow:            rgba(0,0,0,0.5);
      --color-placeholder:       #4a4840;
      --color-highlight:         rgba(74,106,120,0.25);
    }
    body, .ui { font-family: "DejaVu Sans Mono", monospace; }
    a { color: #4a6a78; }
    a:hover { color: #6a8a98; }
    </style>
  '';

  setupForgejoCustom = pkgs.writeShellScript "setup-forgejo-custom" ''
    mkdir -p /data/forgejo/custom/templates/custom
    cp ${forgejoHeaderTmpl} /data/forgejo/custom/templates/custom/header.tmpl
    chown -R 1000:1000 /data/forgejo/custom
  '';
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
          FORGEJO__DEFAULT__APP_NAME = "studiowmb";
          FORGEJO__DEFAULT__APP_SLOGAN = "open source. self-hosted. yours.";
          FORGEJO__server__DOMAIN = "git.studiowmb.com";
          FORGEJO__server__ROOT_URL = "https://git.studiowmb.com/";
          FORGEJO__server__SSH_DOMAIN = "ssh.studiowmb.com";
          FORGEJO__server__SSH_PORT = "22";
          FORGEJO__server__START_SSH_SERVER = "true";
          FORGEJO__database__DB_TYPE = "sqlite3";
          FORGEJO__ui__DEFAULT_THEME = "forgejo-dark";
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

      mssql = {
        image = "mcr.microsoft.com/mssql/server:2022-latest";
        ports = [ "1433:1433" ];
        environment = {
          ACCEPT_EULA = "Y";
          MSSQL_SA_PASSWORD = "Admin1234Secure";
        };
        volumes = [ "/data/mssql:/var/opt/mssql" ];
      };

      mailpit = {
        image = "axllent/mailpit:latest";
        ports = [ "1025:1025" "8025:8025" ];
        environment = {
          MP_SMTP_AUTH = "hoppscotch:Mailpit1234";
          MP_SMTP_AUTH_ALLOW_INSECURE = "true";
        };
      };

      hoppscotch = {
        image = "hoppscotch/hoppscotch:latest";
        ports = [ "3002:3000" "3100:3100" "3170:3170" ];
        environmentFiles = [ "/etc/hoppscotch.env" ];
      };

    };
  };

  environment.etc."hoppscotch.env" = {
    text = hoppscotchEnv;
    mode = "0400";
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
      "+${setupForgejoCustom}"
    ];
  };

  systemd.services.podman-mssql = {
    requires = [ "data.mount" ];
    after    = [ "data.mount" ];
    serviceConfig.ExecStartPre = [
      "${pkgs.coreutils}/bin/mkdir -p /data/mssql"
      "+${pkgs.coreutils}/bin/chown 10001:0 /data/mssql"
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
    allowedTCPPorts = [ 22 3000 2222 5000 6000 8978 3002 3100 3170 1025 8025 1433 ];
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

  boot.loader.grub.device = lib.mkForce "/dev/sda";

  system.stateVersion = "25.11";
}

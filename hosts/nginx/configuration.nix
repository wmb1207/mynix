{ config, pkgs, cloudflaredToken ? "", studiowmbPkg, ... }:

let
  upstream = "192.168.88.38";
  cert     = config.wmbArpa.cert;

  indexPage = pkgs.writeTextDir "index.html" ''
    <!DOCTYPE html>
    <html lang="en">
    <head>
      <meta charset="UTF-8">
      <meta name="viewport" content="width=device-width, initial-scale=1.0">
      <title>wmb.arpa</title>
      <style>
        * { box-sizing: border-box; margin: 0; padding: 0; }
        body {
          font-family: "DejaVu Sans Mono", monospace;
          background: #1c1a18;
          color: #b5b2a0;
          display: flex;
          flex-direction: column;
          justify-content: center;
          min-height: 100vh;
          padding: 2rem;
        }
        .prompt {
          color: #6a6858;
          margin-bottom: 1rem;
          font-size: 0.9rem;
        }
        .prompt .user { color: #4a6a78; }
        .prompt .host { color: #657050; }
        .prompt .cmd  { color: #b5b2a0; }
        .list {
          display: flex;
          flex-direction: column;
          gap: 0.2rem;
          padding-left: 0;
        }
        a {
          display: flex;
          gap: 1.5rem;
          text-decoration: none;
          font-size: 0.9rem;
          color: #b5b2a0;
          padding: 0.1rem 0;
          white-space: nowrap;
        }
        a:hover .name { color: #4a6a78; text-decoration: underline; }
        .name  { min-width: 12ch; }
        .url   { color: #6a6858; overflow: hidden; text-overflow: ellipsis; }
        .cursor {
          display: inline-block;
          width: 0.55em;
          height: 1em;
          background: #b5b2a0;
          margin-left: 0.1em;
          vertical-align: text-bottom;
          animation: blink 1.1s step-end infinite;
        }
        @keyframes blink { 50% { opacity: 0; } }
      </style>
    </head>
    <body>
      <div class="prompt">
        <span class="user">wmb</span><span>@</span><span class="host">arpa</span><span> ~ $ </span><span class="cmd">open services</span>
      </div>
      <div class="list">
        <a href="https://git.studiowmb.com">
          <span class="name">forgejo</span>
          <span class="url">https://git.studiowmb.com</span>
        </a>
        <a href="https://draw.studiowmb.com">
          <span class="name">excalidraw</span>
          <span class="url">https://draw.studiowmb.com</span>
        </a>
        <a href="https://excalidraw.com">
          <span class="name">excalidraw +</span>
          <span class="url">https://app.excalidraw.com</span>
        </a>
        <a href="https://cloudbeaver.wmb.arpa">
          <span class="name">cloudbeaver</span>
          <span class="url">https://cloudbeaver.wmb.arpa</span>
        </a>
        <a href="https://registry.wmb.arpa">
          <span class="name">registry</span>
          <span class="url">https://registry.wmb.arpa</span>
        </a>
        <a href="https://app.studiowmb.com">
          <span class="name">hoppscotch</span>
          <span class="url">https://app.studiowmb.com</span>
        </a>
        <a href="http://hoppscotch-admin.wmb.arpa">
          <span class="name">hopp-admin</span>
          <span class="url">http://hoppscotch-admin.wmb.arpa</span>
        </a>
        <a href="http://mailpit.wmb.arpa">
          <span class="name">mailpit</span>
          <span class="url">http://mailpit.wmb.arpa</span>
        </a>
        <a href="https://192.168.88.2:8006">
          <span class="name">proxmox</span>
          <span class="url">https://192.168.88.2:8006</span>
        </a>
        <a href="http://nas.wmb.arpa">
          <span class="name">nas</span>
          <span class="url">http://nas.wmb.arpa</span>
        </a>
        <a href="https://youtube.com">
          <span class="name">youtube</span>
          <span class="url">https://youtube.com</span>
        </a>
      </div>
      <div class="prompt" style="margin-top:1rem;">
        <span class="user">wmb</span><span>@</span><span class="host">arpa</span><span> ~ $ </span><span class="cursor"></span>
      </div>
    </body>
    </html>
  '';
in
{
  imports = [ ../../modules/ssh-keys.nix ../../modules/wmb-arpa-ca.nix ];

  networking.hostName = "nginx";

  time.timeZone = "America/Argentina/Cordoba";

  i18n.defaultLocale = "en_US.UTF-8";

  users.users.wmb = {
    isNormalUser = true;
    description = "wmb";
    extraGroups = [ "networkmanager" "wheel" ];
    packages = with pkgs; [];
  };

  services.getty.autologinUser = "wmb";

  environment.systemPackages = with pkgs; [
    curl
    wget
  ];

  services.openssh = {
    enable = true;
    settings = {
      PasswordAuthentication = false;
      PermitRootLogin = "no";
    };
  };

  services.nginx = {
    enable = true;

    virtualHosts."studiowmb.com" = {
      serverAliases = [ "www.studiowmb.com" ];
      locations."/" = {
        root = "${studiowmbPkg}";
        extraConfig = ''
          try_files $uri $uri/ /index.html;
        '';
      };
    };

    virtualHosts."wmb.arpa" = {
      default = true;
      forceSSL = true;
      sslCertificate = cert.certFile;
      sslCertificateKey = cert.keyFile;
      locations."/" = {
        root = "${indexPage}";
        extraConfig = ''
          default_type text/html;
          try_files /index.html =404;
        '';
      };
    };

    virtualHosts."forgejo.wmb.arpa" = {
      forceSSL = true;
      sslCertificate = cert.certFile;
      sslCertificateKey = cert.keyFile;
      locations."/" = {
        proxyPass = "http://${upstream}:3000";
        proxyWebsockets = true;
      };
    };

    virtualHosts."excalidraw.wmb.arpa" = {
      forceSSL = true;
      sslCertificate = cert.certFile;
      sslCertificateKey = cert.keyFile;
      locations."/" = {
        proxyPass = "http://${upstream}:5000";
        proxyWebsockets = true;
      };
    };

    virtualHosts."cloudbeaver.wmb.arpa" = {
      forceSSL = true;
      sslCertificate = cert.certFile;
      sslCertificateKey = cert.keyFile;
      locations."/" = {
        proxyPass = "http://${upstream}:8978";
        proxyWebsockets = true;
      };
    };

    virtualHosts."registry.wmb.arpa" = {
      forceSSL = true;
      sslCertificate = cert.certFile;
      sslCertificateKey = cert.keyFile;
      locations."/" = {
        proxyPass = "http://${upstream}:6000";
        extraConfig = ''
          client_max_body_size 0;
          proxy_read_timeout 900;
        '';
      };
    };

    virtualHosts."mailpit.wmb.arpa" = {
      addSSL = true;
      sslCertificate = cert.certFile;
      sslCertificateKey = cert.keyFile;
      locations."/" = {
        proxyPass = "http://${upstream}:8025";
        proxyWebsockets = true;
      };
    };

    virtualHosts."hoppscotch.wmb.arpa" = {
      addSSL = true;
      sslCertificate = cert.certFile;
      sslCertificateKey = cert.keyFile;
      locations."/" = {
        proxyPass = "http://${upstream}:3002";
        proxyWebsockets = true;
      };
    };

    virtualHosts."hoppscotch-api.wmb.arpa" = {
      addSSL = true;
      sslCertificate = cert.certFile;
      sslCertificateKey = cert.keyFile;
      locations."/" = {
        proxyPass = "http://${upstream}:3170";
        proxyWebsockets = true;
      };
    };

    virtualHosts."hoppscotch-admin.wmb.arpa" = {
      addSSL = true;
      sslCertificate = cert.certFile;
      sslCertificateKey = cert.keyFile;
      locations."/" = {
        proxyPass = "http://${upstream}:3100";
        proxyWebsockets = true;
      };
    };

  };

  # ── Cloudflare Tunnel ─────────────────────────────────────────────────────
  # Token comes from hosts/nginx/cloudflared-token.env (gitignored).
  # Copy hosts/nginx/cloudflared-token.env.example → cloudflared-token.env
  # and fill in your token before rebuilding.
  # Ingress rules are configured in the Cloudflare dashboard.
  environment.etc."cloudflared/token.env" = {
    text = cloudflaredToken;
    mode = "0400";
  };

  environment.etc."cloudflared/config.yml" = {
    text = ''
      ingress:
        - hostname: studiowmb.com
          service: http://localhost:80
        - hostname: www.studiowmb.com
          service: http://localhost:80
        - hostname: git.studiowmb.com
          service: http://${upstream}:3000
        - hostname: draw.studiowmb.com
          service: http://${upstream}:5000
        - hostname: app.studiowmb.com
          service: http://${upstream}:3002
        - hostname: app-api.studiowmb.com
          service: http://${upstream}:3170
        - service: http_status:404
    '';
    mode = "0444";
  };

  systemd.services.cloudflared = {
    description = "Cloudflare Tunnel";
    wantedBy = [ "multi-user.target" ];
    after = [ "network.target" ];
    serviceConfig = {
      ExecStart = "${pkgs.cloudflared}/bin/cloudflared tunnel --no-autoupdate --config /etc/cloudflared/config.yml run";
      EnvironmentFile = "/etc/cloudflared/token.env";
      Restart = "always";
      RestartSec = "5s";
    };
  };

  networking.networkmanager.enable = true;

  services.nginx.streamConfig = ''
    server {
      listen 2222;
      proxy_pass ${upstream}:2222;
    }
  '';

  networking.firewall = {
    enable = true;
    allowedTCPPorts = [ 22 80 443 2222 ];
  };

  nix.settings.require-sigs = false;
  nix.settings.trusted-users = [ "root" "wmb" ];

  security.sudo.wheelNeedsPassword = false;

  services.qemuGuest.enable = true;

  system.stateVersion = "25.11";
}

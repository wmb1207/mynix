{ config, pkgs, lib, ... }:

let
  cert = pkgs.runCommand "wmb-arpa-cert" { buildInputs = [ pkgs.openssl ]; } ''
    mkdir -p $out
    openssl req -x509 -nodes -days 3650 \
      -newkey rsa:2048 \
      -keyout $out/key.pem \
      -out $out/cert.pem \
      -subj "/CN=*.wmb.arpa" \
      -addext "subjectAltName=DNS:*.wmb.arpa,DNS:wmb.arpa"
  '';
in
{
  # Expose cert/key paths so the nginx host can reference them.
  options.wmbArpa.cert = {
    certFile = lib.mkOption { type = lib.types.path; default = "${cert}/cert.pem"; readOnly = true; };
    keyFile  = lib.mkOption { type = lib.types.path; default = "${cert}/key.pem";  readOnly = true; };
  };

  # Trust the cert on every host that imports this module.
  config.security.pki.certificateFiles = [ "${cert}/cert.pem" ];
}

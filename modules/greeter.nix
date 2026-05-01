{ pkgs, crystal-greeter, ... }:

let
  pkg = crystal-greeter.packages.${pkgs.system}.default;
in {
  # Ensure fvwm3 and X session tools are present system-wide so the greeter
  # can find them when building its PATH from the nix store.
  environment.systemPackages = with pkgs; [ fvwm3 xorg.xinit xorg.xauth ];

  # The greeter owns tty1; disable the stock getty there.
  systemd.services."getty@tty1".enable  = false;
  systemd.services."autovt@tty1".enable = false;

  systemd.services.crystal-greeter = {
    description = "Crystal TTY Login Greeter";
    after    = [ "systemd-user-sessions.service" ];
    wantedBy = [ "multi-user.target" ];

    serviceConfig = {
      ExecStart      = "${pkg}/bin/crystal-greeter";
      StandardInput  = "tty";
      StandardOutput = "tty";
      TTYPath        = "/dev/tty1";
      TTYReset       = true;
      TTYVHangup     = true;
      Restart        = "always";
      RestartSec     = "1s";
    };
  };
}

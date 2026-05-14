{ pkgs, crystal-greeter, ... }:

{
  imports = [ crystal-greeter.nixosModules.default ];

  services.crystal-greeter = {
    enable  = true;
    package = crystal-greeter.packages.${pkgs.system}.default;
    # title and menu are left for each host to override
  };

  # Suppress kernel console messages so they don't corrupt the greeter UI.
  # Messages still accumulate in the ring buffer (readable via dmesg / journalctl -k).
  boot.consoleLogLevel = 3;

  # WM packages needed for X session discovery and launch.
  environment.systemPackages = with pkgs; [ fvwm3 xorg.xinit xorg.xauth ];

  # XDG desktop portal + GTK backend.
  # Without this, apps that use the portal API (flameshot, file pickers, etc.)
  # will time out waiting for a portal response — GNOME/KDE set this up
  # automatically but a bare WM session does not.
  xdg.portal.enable = true;
  xdg.portal.extraPortals = [ pkgs.xdg-desktop-portal-gtk ];
  xdg.portal.config.common.default = "*";
}

{ config, pkgs, lib, inputs, ... }:
let
  dag = inputs.home-manager.lib.hm.dag;
  cli = import ./cli.nix { inherit pkgs; };
  iac = import ./development/infrastructure.nix { inherit pkgs; };
  gui = import ./gui.nix { inherit pkgs; };
  fonts = import ./fonts.nix { inherit pkgs; };
  wm-tools = import ./wm-tools.nix { inherit pkgs; };
  programming-languages = import ./development/programming-languages.nix { inherit pkgs; };

  myEmacs = pkgs.emacs.pkgs.withPackages (epkgs: with epkgs; [
  lsp-mode
    use-package
    vterm
  ]);
in
{
  users.users.wmb = {
    isNormalUser = true;
    description = "wmb";
    extraGroups = [ "networkmanager" "wheel" "docker" "audio"];
  };

  services.xserver.windowManager.bspwm.enable = true;
  services.xserver.enable = true;
  services.xserver.xautolock = {
    enable = true;
    time = 10;
    locker = "${pkgs.xsecurelock}/bin/xsecurelock";
  };
  
  services.xserver.displayManager.startx.enable = true;
  services.udev.packages = [
    pkgs.steamPackages.steam
  ];

  home-manager.users.wmb = { pkgs, ... }: {
    nixpkgs.config.allowUnfree = true;
    home.stateVersion = "24.11";
    programs.bash.enable = true;

    home.packages = cli ++ programming-languages ++ gui ++ fonts ++ iac ++ wm-tools ++ [
    	pkgs.acpi
	    pkgs.networkmanager
	    pkgs.xorg.xmodmap
      pkgs.xsecurelock
      pkgs.picom
    ];

    programs.emacs.enable = true;
    programs.emacs.package = myEmacs;

    home.sessionVariables = {
      EDITOR = "emacs";
    };

    programs.home-manager.enable = true;

    xsession.enable = true;

    # home.file.".emacs.d/init.el".source = ../assets/init.el;
    home.activation.initEl = dag.entryAfter ["writeBoundary"] ''
      mkdir -p "$HOME/.emacs.d/lisp" &&
    	cp ${../assets/init.el} "$HOME/.emacs.d/init.el" &&
      cp ${../assets/packages.el} "$HOME/.emacs.d/lisp/packages.el"
    '';

    home.file.".bashrc".source = lib.mkForce ../assets/bashrc;
    home.file.".config/ghostty/config".source = ../assets/ghostty;
    home.file.".config/bspwm/bspwmrc" = {
      source = ../assets/bspwmrc;
      executable = true;
    };
    home.file.".config/sxhkd/sxhkdrc".source = ../assets/sxhkdrc;
    home.file.".xinitrc".source = ../assets/xinitrc;
    home.file.".config/polybar/config.ini".source = ../assets/polybar.ini;
    home.file.".config/polybar/launch.sh" = {
      source = ../assets/polybar-start.sh;
      executable = true;
    };
    home.file.".lemonbar.sh" = {
      source = ../assets/lemonbar.sh;
      executable = true;
    };
    home.file.".config/picom/picom.conf".source = ../assets/picom.conf;
    home.file.".config/wallpapers/gradient.png".source = ../assets/gradient.png;
  };
}

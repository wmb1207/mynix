{ config, pkgs, lib, ... }:

let
  cli = import ./cli.nix { inherit pkgs; };
  programming-languages = import ./programming-languages.nix { inherit pkgs; };
  gui = import ./gui.nix { inherit pkgs; };

  myEmacs = pkgs.emacs.pkgs.withPackages (epkgs: with epkgs; [
    exwm
    use-package
    vterm
  ]);

in
{
  users.users.wmb = {
    isNormalUser = true;
    description = "wmb";
    extraGroups = [ "networkmanager" "wheel" "docker" ];
  };


  # ✅ System-wide configuration
  services.xserver.windowManager.bspwm.enable = true;  # ✅ This is correct
  
  services.xserver.enable = true;
  services.xserver.displayManager.startx.enable = true;
  services.xserver.windowManager.session = [
    {
      name = "emacs";
      start = ''
        ${myEmacs}/bin/emacs
      '';
    }
  ];

  
  # ✅ This needs to be outside of `users.users`
  home-manager.users.wmb = { pkgs, ... }: {
    nixpkgs.config.allowUnfree = true;
    home.stateVersion = "24.11";
    programs.bash.enable = true;

    home.packages = cli ++ programming-languages ++ gui ++ [
      pkgs.awscli
      pkgs.terraform
      pkgs.opentofu
      pkgs.ghostty
      pkgs.cherry
      pkgs.xterm
      pkgs.picom
      pkgs.feh
      pkgs.dmenu
    ];

    programs.emacs.enable = true;
    programs.emacs.package = myEmacs;

    home.sessionVariables = {
      EDITOR = "emacs";
    };

    programs.home-manager.enable = true;

    home.file.".emacs.d/init.el".source = ../assets/init.el;
    home.file.".bashrc".source = lib.mkForce ../assets/bashrc;
    home.file.".config/ghostty/config".source = ../assets/ghostty;

    xsession.enable = true;

      # Basic config files
    home.file.".config/bspwm/bspwmrc".source = ../assets/bspwmrc;

    home.file.".config/sxhkd/sxhkdrc".source = ../assets/skhdrc;

    # Optional: Start Emacsclient or daemon in X session
    home.file.".xinitrc".source = ../assets/xinitrc;
  };
}

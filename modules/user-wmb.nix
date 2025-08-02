{ config, pkgs, lib, inputs, system, ... }:
let
  dag = inputs.home-manager.lib.hm.dag;
  cli = import ./cli.nix { inherit pkgs; };
  iac = import ./development/infrastructure.nix { inherit pkgs; };
  gui = import ./gui.nix { inherit pkgs; inherit system; };
  fonts = import ./fonts.nix { inherit pkgs; };
  wm-tools = import ./wm-tools.nix { inherit pkgs; };
  programming-languages = import ./development/programming-languages.nix { inherit pkgs; };

  myEmacs = pkgs.emacs.pkgs.withPackages (epkgs: with epkgs; [
  lsp-mode
    use-package
    vterm
  ]);
  
  grammars = [
    pkgs.tree-sitter-grammars.tree-sitter-php
    pkgs.tree-sitter-grammars.tree-sitter-typescript
    pkgs.tree-sitter-grammars.tree-sitter-tsx
    pkgs.tree-sitter-grammars.tree-sitter-python
    pkgs.tree-sitter-grammars.tree-sitter-rust
    pkgs.tree-sitter-grammars.tree-sitter-clojure
    pkgs.tree-sitter-grammars.tree-sitter-go
    pkgs.tree-sitter-grammars.tree-sitter-elixir
  ];

  treeSitterLibDir = pkgs.linkFarm "tree-sitter-libs" (builtins.concatLists (map (grammar:
    let
      path = grammar + "/parser";
      hasParser = builtins.pathExists path;
    in
      if hasParser then
        [{ name = "lib${grammar.pname}.so"; path = path; }]
      else
        []
  ) grammars));
in
{
  users.users.wmb = {
    isNormalUser = true;
    description = "wmb";
    extraGroups = [ "networkmanager" "wheel" "docker" "audio"];
  };

  services.cloudflare-warp.enable = true;
  services.xserver.windowManager.bspwm.enable = true;
  services.xserver.enable = true;
  services.xserver.xautolock = {
    enable = true;
    time = 10;
    locker = "${pkgs.xsecurelock}/bin/xsecurelock";
  };
  
  services.xserver.displayManager.startx.enable = true;
  services.udev.packages = if lib.hasAttr "steamPackages" pkgs then
    lib.optional (!builtins.elem system [ "aarch64-linux" ]) pkgs.steamPackages.steam
                           else
                             [];

#  services.udev.packages = [
#    pkgs.steamPackages.steam
#  ];

  home-manager.users.wmb = { pkgs, ... }: {
    nixpkgs.config.allowUnfree = true;
    home.stateVersion = "24.11";
    programs.bash.enable = true;

    home.packages = cli ++ programming-languages ++ gui ++ fonts ++ iac ++ wm-tools ++ [
      pkgs.cloudflare-warp
    	pkgs.acpi
	    pkgs.networkmanager
	    pkgs.xorg.xmodmap
      pkgs.xsecurelock
      pkgs.picom
      pkgs.typescript
    ];

    programs.emacs.enable = true;
    programs.emacs.package = myEmacs;

    home.sessionVariables = {
      EDITOR = "emacs";
      TREE_SITTER_LIBDIR = "${treeSitterLibDir}";
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
    home.file.".config/wallpapers/galaxy-plant.jpeg".source = ../assets/galaxy-plant.jpeg;
    home.file.".config/wallpapers/wallpaper-1.jpg".source = ../assets/wallpaper-1.jpg;
    home.file.".config/wallpapers/wallpaper-2.jpg".source = ../assets/wallpaper-2.jpg;
    home.file.".config/wallpapers/wallpaper-3.jpg".source = ../assets/wallpaper-3.jpg;
    home.file.".config/wallpapers/wallpaper-4.jpg".source = ../assets/wallpaper-4.jpg;
    home.file.".config/wallpapers/wallpaper-5.jpg".source = ../assets/wallpaper-5.jpg;
    home.file.".config/wallpapers/wallpaper-6.jpg".source = ../assets/wallpaper-6.jpg;
    home.file.".config/wallpapers/wallpaper-7.jpg".source = ../assets/wallpaper-7.jpg;
    home.file.".config/dunst/dunstrc".source = ../assets/dunstrc;
    home.file.".bin/battery.sh".source = ../assets/battery.sh;
  };
}

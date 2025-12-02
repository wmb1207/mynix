{ config, pkgs, lib, inputs, system, ... }:
let
  dag = inputs.home-manager.lib.hm.dag;
  cli = import ./cli.nix { inherit pkgs; };
  iac = import ./development/infrastructure.nix { inherit pkgs; };
  gui = import ./gui.nix { inherit pkgs; inherit system; };
  my-fonts = import ./fonts.nix { inherit pkgs; };
  wm-tools = import ./wm-tools.nix { inherit pkgs; };
  programming-languages = import ./development/programming-languages.nix { inherit pkgs; };
  assets = import ./asseter.nix { inherit lib; };
  myEmacs = pkgs.emacs-gtk.pkgs.withPackages (epkgs: with epkgs; [
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
    pkgs.tree-sitter-grammars.tree-sitter-ocaml
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
  services.emacs = {
    enable = true;
    package = myEmacs;
  };
  services.xserver.enable = true;
  services.xserver.xautolock = {
    enable = true;
    time = 2;
    locker = "${pkgs.xsecurelock}/bin/xsecurelock";
  };
   
  services.xserver.displayManager.startx.enable = true;
  services.udev.packages = if lib.hasAttr "steamPackages" pkgs then
    lib.optional (!builtins.elem system [ "aarch64-linux" ]) pkgs.steamPackages.steam
  else
    [];
  
  fonts.packages = my-fonts;
  
  home-manager.users.wmb = { pkgs, ... }: {
    xsession.enable = true;
    nixpkgs.config.allowUnfree = true;
    home.stateVersion = "25.05";
    programs.bash.enable = true;
    
    home.packages = cli ++ programming-languages ++ gui ++ iac ++ wm-tools ++ [
      pkgs.cloudflare-warp
      pkgs.acpi
      pkgs.networkmanager
      pkgs.xorg.xmodmap
      pkgs.xsecurelock
      pkgs.picom
      pkgs.typescript
    ];
    
    gtk = {
      enable = true;
      theme = {
        name = "Gruvbox-Dark-B";
        package = pkgs.gruvbox-dark-gtk;
      };
    };
    
    # programs.emacs = {
    #   enable = true;
    #   package = myEmacs;
    # };
    
    home.sessionVariables = {
      EDITOR = "emacs";
      TREE_SITTER_LIBDIR = "${treeSitterLibDir}";
      GPUI_X11_SCALE_FACTOR = "1";
    };
    
    programs.home-manager.enable = true;
    
    home.activation.initEl = dag.entryAfter ["writeBoundary"] ''
      mkdir -p "$HOME/.emacs.d/lisp" &&
      ln -s ${../assets/init.el} "$HOME/.emacs.d/init.el" &&
      ln -s ${../assets/packages.el} "$HOME/.emacs.d/lisp/packages.el"
    '';
    
    home.file = assets.wallpapers 10 // assets.assets;
  };
}

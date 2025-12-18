{ config, pkgs, lib, inputs, system, ... }:

let
  dag = inputs.home-manager.lib.hm.dag;

  cli  = import ./cli.nix { inherit pkgs; };
  iac  = import ./development/infrastructure.nix { inherit pkgs; };
  gui  = import ./gui.nix { inherit pkgs system; };
  my-fonts = import ./fonts.nix { inherit pkgs; };
  wm-tools = import ./wm-tools.nix { inherit pkgs; };
  programming-languages =
    import ./development/programming-languages.nix { inherit pkgs; };
  assets = import ./asseter.nix { inherit lib; };

  myEmacs =
    let
      customEmacs = pkgs.emacs30.overrideAttrs (old: {
        buildInputs = old.buildInputs ++ [ pkgs.gtk3 ];
        configureFlags = old.configureFlags ++ [
          "--with-gnutls"
          "--with-x"
          "--with-native-compilation"
          "--with-png"
          "--with-jpeg"
          "--with-sound"
          "--with-libsystemd"
          "--with-harfbuzz"
          "--with-json"
          "--with-dbus"
          "--with-file-notification=inotify"
          "--with-wide-int"
          "--with-pdumper=yes"
        ];
      });

      emacsPkgSet = pkgs.emacsPackagesFor customEmacs;
    in
      emacsPkgSet.emacsWithPackages (epkgs: [
        epkgs.use-package
        epkgs.lsp-mode
        epkgs.vterm
      ]);

  grammars = with pkgs.tree-sitter-grammars; [
    tree-sitter-php
    tree-sitter-typescript
    tree-sitter-tsx
    tree-sitter-python
    tree-sitter-rust
    tree-sitter-clojure
    tree-sitter-go
    tree-sitter-elixir
    tree-sitter-ocaml
  ];

  treeSitterLibDir =
    pkgs.linkFarm "tree-sitter-libs"
      (builtins.concatLists (map (grammar:
        let
          path = grammar + "/parser";
        in
          if builtins.pathExists path then
            [{ name = "lib${grammar.pname}.so"; path = path; }]
          else
            []
      ) grammars));
in
{
  users.users.wmb = {
    isNormalUser = true;
    description = "wmb";
    extraGroups = [ "wheel" "networkmanager" "docker" "audio" ];
  };

  #### Console #############################################################

  console = {
    font = "ter-v12n";
    packages = [ pkgs.terminus_font ];
    earlySetup = true;
  };

  #### System services #####################################################

  services.emacs = {
    enable = true;
    package = myEmacs;
  };

  programs.dconf.enable = true;
  security.polkit.enable = true;

  hardware.bluetooth = {
    enable = true;
    powerOnBoot = true;
  };

  #### greetd + tuigreet ###################################################

  services.greetd = {
    enable = true;
    settings.default_session = {
      user = "greeter";
      command = ''
        ${pkgs.greetd.tuigreet}/bin/tuigreet \
          --time \
          --remember \
          --cmd "dbus-run-session startx"
      '';
    };
  };

  #### X server ############################################################

  services.xserver.enable = true;

  services.xserver.displayManager.startx.enable = true;

  services.xserver.xautolock = {
    enable = true;
    time = 2;
    locker = "${pkgs.xsecurelock}/bin/xsecurelock";
  };

  services.xserver.windowManager = {
    bspwm.enable = true;
    windowmaker.enable = true;
    fvwm3.enable = true;
  };


  #### udev ################################################################

  services.udev.packages =
    if lib.hasAttr "steamPackages" pkgs then
      lib.optional (!builtins.elem system [ "aarch64-linux" ])
        pkgs.steamPackages.steam
    else
      [];

  #### Fonts ###############################################################

  fonts.packages = my-fonts;

  #### Home Manager ########################################################

  home-manager.users.wmb = { pkgs, ... }: {
    home.stateVersion = "25.05";

    nixpkgs.config.allowUnfree = true;

    programs.bash.enable = true;
    programs.home-manager.enable = true;

    xsession.enable = true;
    xresources.properties = {
      "XTerm*faceName" = "Monospace";
      "XTerm*faceSize" = 10;

      "XTerm*foreground" = "#ffffff";
      "XTerm*background" = "#000000";
      "XTerm*cursorColor" = "#00ff00";

      "XTerm*scrollBar" = false;
      "XTerm*internalBorder" = 2;
      "XTerm*saveLines" = 1000;
      "XTerm*borderWidth" = 0;

      "XTerm*metaSendsEscape" = true;
      "XTerm*eightBitInput" = false;

      "XTerm*selectToClipboard" = false;
      "XTerm*cutNewline" = true;
    };
    xresources.extraConfig = ''
      XTerm*VT100.Translations: #override \
      Ctrl <Key>w: cut-selection() \n\
      Meta <Key>w: copy-selection() \n\
      Ctrl <Key>y: insert-selection()
    '';


    home.packages =
      cli
      ++ programming-languages
      ++ gui
      ++ iac
      ++ wm-tools
      ++ [
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

    home.sessionVariables = {
      EDITOR = "emacs";
      TREE_SITTER_LIBDIR = "${treeSitterLibDir}";
      GPUI_X11_SCALE_FACTOR = "1";
    };

    home.activation.initEl =
      dag.entryAfter [ "writeBoundary" ] ''
        mkdir -p "$HOME/.emacs.d/lisp"
        ln -sf ${../assets/init.el} "$HOME/.emacs.d/init.el"
        ln -sf ${../assets/packages.el} "$HOME/.emacs.d/lisp/packages.el"
      '';

    home.file = assets.wallpapers 10 // assets.assets;
  };
}

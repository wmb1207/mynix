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
  
  treeSitterLibDir =
    pkgs.linkFarm "tree-sitter-libs" [
      {
        name = "libtree-sitter-scala.so";
        path = "${pkgs.tree-sitter-grammars.tree-sitter-scala}/parser";
      }
    ];
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
  # Font configuration
  "URxvt.font" = "xft:CozetteVector:size=14";
  "URxvt.boldFont" = "xft:CozetteVector:bold:size=14";
  "URxvt.italicFont" = "xft:CozetteVector:italic:size=14";
  
  # Colors
/* Couleurs Tango */
"URxvt.foreground" ="#C8C8C8";
"URxvt.background" ="#323232";
"URxvt.color0" =    "#2E3436";
"URxvt.color1" =    "#CC0000";
"URxvt.color2" =    "#4E9A06";
"URxvt.color3" =    "#C4A000";
"URxvt.color4" =    "#3465A4";
"URxvt.color5" =    "#75507B";
"URxvt.color6" =    "#06989A";
"URxvt.color7" =    "#D3D7CF";
"URxvt.color8" =    "#555753";
"URxvt.color9" =    "#EF2929";
"URxvt.color10" =   "#8AE234";
"URxvt.color11" =   "#FCE94F";
"URxvt.color12" =   "#729FCF";
"URxvt.color13" =   "#AD7FA8";
"URxvt.color14" =   "#34E2E2";
"URxvt.color15" =   "#EEEEEC";
"URxvt.scrollBar" =        false;
"URxvt.scrollTtyOutput" =  false;
"URxvt.scrollWithBuffer" = true;
"URxvt.scrollTtyKeypress" = true;
  # Border and padding
  "URxvt.internalBorder" = 2;
  "URxvt.borderWidth" = 0;
  
  # Scrollback
  "URxvt.saveLines" = 1000;
  
  # Clipboard and selection
  "URxvt.perl-ext-common" = "default,clipboard,selection-to-clipboard";
  "URxvt.clipboard.autocopy" = true;
  "URxvt.clipboard.copycmd" = "xclip -i -selection clipboard";
  "URxvt.clipboard.pastecmd" = "xclip -o -selection clipboard";
  
  # Meta key behavior (equivalent to metaSendsEscape)
  "URxvt.meta8" = false;
  
  # ISO 14755 mode (disable for cleaner input)
  "URxvt.iso14755" = false;
  "URxvt.iso14755_52" = false;
};

xresources.extraConfig = ''
  URxvt.keysym.Control-Shift-w: perl:clipboard:copy
  URxvt.keysym.Control-Shift-y: perl:clipboard:paste
'';

programs.bash.initExtra = ''
  bind '"\C-w":""'
  bind '"\C-y":""'
'';
#     xresources.properties = {
#       "XTerm*faceName" = "CozetteVector";
#       "XTerm*faceSize" = 12;

#       "XTerm*foreground" = "#ffffff";
#       "XTerm*background" = "#000000";
#       "XTerm*cursorColor" = "#00ff00";

#       "XTerm*scrollBar" = false;
#       "XTerm*internalBorder" = 2;
#       "XTerm*saveLines" = 1000;
#       "XTerm*borderWidth" = 0;

#       "XTerm*metaSendsEscape" = true;
#       "XTerm*eightBitInput" = false;

#       "XTerm*selectToClipboard" = true;
#       "XTerm*cutNewline" = true;
#     };

#     xresources.extraConfig = ''
#   XTerm*VT100.Translations: #override \
#     Ctrl Shift <Key>w: copy-selection(CLIPBOARD, PRIMARY, CUT_BUFFER0) \n\
#     Ctrl Shift <Key>y: insert-selection(CLIPBOARD, PRIMARY) \n
# '';
   
    # programs.bash.initExtra = ''
    #     bind '"\C-w":""'
    #     bind '"\C-y":""'
#       '';


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
        pkgs.tree-sitter
        (pkgs.symlinkJoin {
          name = "tree-sitter-grammars";
          paths = with pkgs.tree-sitter-grammars; [
            tree-sitter-php
            tree-sitter-typescript
            tree-sitter-tsx
            tree-sitter-python
            tree-sitter-rust
            tree-sitter-clojure
            tree-sitter-go
            tree-sitter-elixir
            tree-sitter-ocaml
            tree-sitter-scala
          ];
        })
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
        
      # TREE_SITTER_LIBDIR = "${treeSitterLibDir}";
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

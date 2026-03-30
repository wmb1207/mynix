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
      emacsPkgSet.emacsWithPackages (epkgs: with epkgs; [
        # Core packages
        use-package

        # LSP and completion
        lsp-mode
        lsp-ui
        lsp-pyright
        lsp-metals

        # Terminal
        vterm
        multi-vterm
        eat

        # File and project management
        magit
        direnv
        envrc
        editorconfig
        dired-sidebar
        ibuffer-sidebar
        neotree
        treemacs
        dired-subtree

        # Completion and navigation
        vertico
        vertico-posframe
        consult
        ace-window

        # Programming languages
        ## Python
        python-black
        py-isort
        python-isort
        pyvenv
        pet

        ## janet
        janet-mode

        ## Go
        go-mode

        ## Elixir
        elixir-mode
        inf-elixir

        ## PHP
        php-mode

        ## TypeScript/JavaScript
        typescript-mode
        rjsx-mode
        web-mode
        prettier

        ## Clojure
        clojure-mode
        clojure-ts-mode
        cider
        inf-clojure

        ## Scala
        scala-ts-mode
        sbt-mode

        ## OCaml
        tuareg
        merlin
        merlin-eldoc

        ## Ruby
        inf-ruby

        ## Nix
        nix-mode

        ## Other
        dockerfile-mode
        terraform-mode
        yaml-mode

        # Debuggers
        dap-mode
        dape
        realgud
        indium

        # Flycheck
        flycheck-inline
        flycheck-golangci-lint

        # Themes
        mbo70s-theme
        gruber-darker-theme
        base16-theme
        plan9-theme
        doric-themes
        parchment-theme
        srcery-theme
        arjen-grey-theme
        sublime-themes
        creamsody-theme
        acme-theme
        ef-themes
        ample-theme
        autothemer
        punpun-themes

        # UI enhancements
        beframe
        persp-mode
        rainbow-delimiters
        solaire-mode

        # Org mode
        org
        org-present
        ob-restclient
        verb

        # Tools
        exec-path-from-shell
        gptel
        detached
        load-env-vars
        emms
        ein
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
    shell = pkgs.loksh;
    extraGroups = [ "networkmanager" "wheel" "docker" "audio" "input" "video" ];
  };

  services.udev.packages = [pkgs.game-devices-udev-rules];

  services.udev.extraRules = ''
                           KERNEL=="hidraw*", ATTRS{idVendor}=="054c", ATTRS{idProduct}=="0ce6", MODE="0660", TAG+="uaccess"
                           KERNEL=="hidraw*", KERNELS=="*054C:0CE6*", MODE="0660", TAG+="uaccess"
                           ACTION=="add", SUBSYSTEM=="backlight", RUN+="${pkgs.coreutils}/bin/chgrp video /sys/class/backlight/%k/brightness", RUN+="${pkgs.coreutils}/bin/chmod g+w /sys/class/backlight/%k/brightness"
'';
  
  services.emacs = {
    enable = true;
    package = myEmacs;
  };

  programs.dconf.enable = true;
  services.tailscale.enable = true;
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
    fvwm3.enable = true;
  };

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
      # Font configuration (unchanged)
      "URxvt.font" = "xft:DejaVu Sans Mono:size=10";
      "URxvt.boldFont" = "xft:DejaVu Sans Mono:bold:size=10";
      "URxvt.italicFont" = "xft:DejaVu Sans Mono:italic:size=10";

      # ── Colors (gloomy creamsody) ──
      "URxvt.foreground" = "#b5b2a0"; # default fg
      "URxvt.background" = "#1c1a18"; # default bg

      # ANSI palette
      "URxvt.color0"  = "#1c1a18"; # black
      "URxvt.color1"  = "#884545"; # red (deep rose)
      "URxvt.color2"  = "#657050"; # green (olive)
      "URxvt.color3"  = "#8a7040"; # yellow (dark amber)
      "URxvt.color4"  = "#4a6a78"; # blue (steel)
      "URxvt.color5"  = "#785a5a"; # magenta (mauve)
      "URxvt.color6"  = "#4a7070"; # cyan (muted teal)
      "URxvt.color7"  = "#9a9888"; # white (warm grey)

      "URxvt.color8"  = "#3a3830"; # bright black (dim)
      "URxvt.color9"  = "#9a5035"; # bright red (burnt sienna)
      "URxvt.color10" = "#7a8060"; # bright green (lighter olive)
      "URxvt.color11" = "#9a8050"; # bright yellow (golden tan)
      "URxvt.color12" = "#5a7888"; # bright blue (lighter steel)
      "URxvt.color13" = "#8a7070"; # bright magenta (dusty rose)
      "URxvt.color14" = "#5a8080"; # bright cyan (lighter teal)
      "URxvt.color15" = "#b5b2a0"; # bright white (cream)

      # Cursor & selection
      "URxvt.cursorColor" = "#b0ad9a";
      "URxvt.highlightColor" = "#3a3525";
      "URxvt.highlightTextColor" = "#b5b2a0";

      # Scrollbar & scrolling (unchanged)
      "URxvt.scrollBar" = false;
      "URxvt.scrollTtyOutput" = false;
      "URxvt.scrollWithBuffer" = true;
      "URxvt.scrollTtyKeypress" = true;

      # Border and padding (slightly tighter, Acme feel)
      "URxvt.internalBorder" = 2;
      "URxvt.borderWidth" = 0;

      # Scrollback
      "URxvt.saveLines" = 1000;

      # Clipboard and selection
      "URxvt.perl-ext-common" = "default,clipboard,selection-to-clipboard";
      "URxvt.clipboard.autocopy" = true;
      "URxvt.clipboard.copycmd" = "xclip -i -selection clipboard";
      "URxvt.clipboard.pastecmd" = "xclip -o -selection clipboard";

      # Meta key behavior
      "URxvt.meta8" = false;

      # ISO 14755 mode
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
    home.packages =
      cli
      ++ programming-languages
      ++ gui
      ++ iac
      ++ wm-tools
      ++ [ myEmacs
        
        (pkgs.writeShellScriptBin "Ldef" "exec L def \"$@\"")
        (pkgs.writeShellScriptBin "Lrefs" "exec L refs \"$@\"")
        (pkgs.writeShellScriptBin "Lrn" "exec L rn \"$@\"")
        (pkgs.writeShellScriptBin "Lassist" "exec L assist \"$@\"")

        #pkgs.acme-lsp
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

    # === Session Variables ===
    home.sessionVariables = {
      EDITOR = "emacsclient -c -a emacs";
      VISUAL = "$EDITOR";
      PAGER = "less -R";
      MANPAGER = "sh -c 'col -bx | bat -l man -p'";
      TREE_SITTER_LIBDIR = "${treeSitterLibDir}";
        
      # TREE_SITTER_LIBDIR = "${treeSitterLibDir}";
      GPUI_X11_SCALE_FACTOR = "1";
      ENV = "$HOME/.kshrc";
    };
    
    # === Session Path ===
    home.sessionPath = [
      "$HOME/.local/bin"
      "$HOME/go/bin"
      "$HOME/.cargo/bin"
    ];
    
    home.file = assets.wallpapers 11  // assets.assets // {
      ".kshrc".text = ''
  # Only interactive shells   
  [[ $- != *i* ]] && return   
  
  # === History ===
  HISTFILE="$HOME/.ksh_history"
  HISTSIZE=10000
  SAVEHIST=20000
  
  PS1='\033[38;5;67m$(pwd | sed "s|$HOME|~|")\033[0m $ '
  
  # === Aliases ===
  alias ll='ls -lh --color=auto'
  alias la='ls -lah --color=auto'
  alias gs='git status'
  alias gc='git commit'
  alias ga='git add'
  alias gl='git log --oneline --graph --decorate'
  alias em='emacsclient -nw'
  alias e='emacsclient -c -a emacs'
  alias ns='nix-shell'
  alias nb='nix build'
  alias nr='nixos-rebuild switch --flake .#'
  alias nf='nix flake show'
  alias nh='nix develop'
  
  # === fzf ===
  if command -v fzf >/dev/null 2>&1; then
    export FZF_DEFAULT_COMMAND='rg --files --hidden --follow --glob "!.git/*"'
    export FZF_CTRL_T_COMMAND="$FZF_DEFAULT_COMMAND"
    export FZF_DEFAULT_OPTS="--height 40% --layout=reverse --border"
  fi
  
  # === Direnv ===
  ${lib.optionalString config.programs.direnv.enable ''
    eval "$(direnv hook ksh)"
  ''}
  
  # === Starship ===
  ${lib.optionalString config.programs.starship.enable ''
    eval "$(starship init ksh)"
  ''}
'';
    };
  };
}

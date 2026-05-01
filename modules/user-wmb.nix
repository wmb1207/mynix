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

  # ── Themes ──────────────────────────────────────────────────────────────────

  themes = import ./themes.nix {};
  theme  = themes.dark;

  # Render an fvwm-style {{var}} template using an attrset of substitutions
  renderTemplate = tmplPath: vars:
    let
      keys  = builtins.attrNames vars;
      froms = map (k: "{{${k}}}") keys;
      tos   = map (k: builtins.toString vars.${k}) keys;
    in
      builtins.replaceStrings froms tos (builtins.readFile tmplPath);

  # Generate dunstrc from a theme attrset
  dunstrcFor = t: ''
    [global]
        font = "${t.font} 10"
        background = "${t.bgAlt}"
        foreground = "${t.comments}"
        frame_color = "${t.active}"
        separator_color = "${t.bgAlt}"
        width = 150
        offset=0x0
        horizontal_padding = 0
        padding = 0
        frame_width = 2
        gap_size = 10
        origin = center-left
        alignment = center
        show_indicators = false

    [urgency_low]
        background = "${t.bgAlt}"
        foreground = "${t.comments}"
        frame_color = "${t.active}"
        timeout = 10

    [urgency_normal]
        background = "${t.bgAlt}"
        foreground = "${t.foreground}"
        frame_color = "${t.comments}"
        timeout = 10

    [urgency_critical]
        background = "${t.bgAlt}"
        foreground = "${t.color1}"
        frame_color = "${t.color1}"
        timeout = 0

    [spotify]
        appname = "Spotify"
        background = "${t.bgAlt}"
        foreground = "${t.comments}"
        frame_color = "${t.comments}"
        timeout = 5
        format = "<b>%s</b>\n%b"
        alignment = center
        word_wrap = yes

    [spotify_alt]
        desktop_entry = "spotify"
        background = "${t.bgAlt}"
        foreground = "${t.comments}"
        frame_color = "${t.comments}"
        timeout = 5
  '';

  # Serialize a theme attrset to EDN for theme.clj to consume at runtime
  themeToEdn = t: ''
    {:name           "${t.name}"
     :font           "${t.font}"
     :background     "${t.background}"
     :foreground     "${t.foreground}"
     :active         "${t.active}"
     :bg-alt         "${t.bgAlt}"
     :active-alt     "${t.activeAlt}"
     :olive          "${t.olive}"
     :comments       "${t.comments}"
     :selection      "${t.selection}"
     :emacs-theme    "${t.emacsTheme}"
     :colors         ["${t.color0}"  "${t.color1}"  "${t.color2}"  "${t.color3}"
                      "${t.color4}"  "${t.color5}"  "${t.color6}"  "${t.color7}"
                      "${t.color8}"  "${t.color9}"  "${t.color10}" "${t.color11}"
                      "${t.color12}" "${t.color13}" "${t.color14}" "${t.color15}"]
     :cursor         "${t.cursor}"
     :highlight      "${t.highlight}"
     :highlight-text "${t.highlightText}"}
  '';

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

        ## Rust
        rust-mode

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
    extraGroups = [ "networkmanager" "wheel" "audio" "input" "video" ];
    openssh.authorizedKeys.keyFiles =
      lib.optional (builtins.pathExists ../secrets/wmb.pub) ../secrets/wmb.pub;
  };

  services.udev.packages = [pkgs.game-devices-udev-rules];

  services.udev.extraRules = ''
                           KERNEL=="hidraw*", ATTRS{idVendor}=="054c", ATTRS{idProduct}=="0ce6", MODE="0660", TAG+="uaccess"
                           KERNEL=="hidraw*", KERNELS=="*054C:0CE6*", MODE="0660", TAG+="uaccess"
                           ACTION=="add", SUBSYSTEM=="backlight", RUN+="${pkgs.coreutils}/bin/chgrp video /sys/class/backlight/%k/brightness", RUN+="${pkgs.coreutils}/bin/chmod g+w /sys/class/backlight/%k/brightness"
'';
  
  programs.dconf.enable = true;
  services.tailscale.enable = true;
  security.polkit.enable = true;

  hardware.bluetooth = {
    enable = true;
    powerOnBoot = true;
  };

  #### X server ############################################################

  services.xserver.enable = true;

  services.xserver.displayManager.startx.enable = true;

  services.xserver.windowManager = {
    fvwm3.enable = true;
  };

  #### Fonts ###############################################################

  fonts.packages = my-fonts;

  #### Home Manager ########################################################

  home-manager.users.wmb = { pkgs, config, ... }: {
    home.stateVersion = "25.05";

    nixpkgs.config.allowUnfree = true;

    programs.bash.enable = true;

    programs.home-manager.enable = true;

    programs.tmux = {
      enable = true;
      prefix = "C-b";
      keyMode = "emacs";
      escapeTime = 0;
      historyLimit = 50000;
      mouse = true;
      terminal = "screen-256color";
      plugins = with pkgs.tmuxPlugins; [
        {
          plugin = resurrect;
          extraConfig = ''
            set -g @resurrect-capture-pane-contents 'on'
          '';
        }
      ];
      extraConfig = ''
        set -g status-keys emacs

        # Send literal C-b with C-b C-b
        bind C-b send-prefix

        # Emacs-style splits (C-b 2 / C-b 3)
        unbind '"'
        unbind %
        bind 2 split-window -v -c "#{pane_current_path}"
        bind 3 split-window -h -c "#{pane_current_path}"

        # Emacs window/pane management
        bind 0 kill-pane
        bind 1 resize-pane -Z
        bind o select-pane -t :.+
        bind O select-pane -t :.-
        bind k kill-window
        bind b choose-window

        # New window keeping current path
        bind c new-window -c "#{pane_current_path}"

        # Window navigation
        bind n next-window
        bind p previous-window

        # Copy mode — pipe through xclip into X clipboard
        bind [ copy-mode
        bind ] paste-buffer
        bind-key -T copy-mode M-w send-keys -X copy-pipe-and-cancel "xclip -in -selection clipboard"
        bind-key -T copy-mode C-w send-keys -X copy-pipe-and-cancel "xclip -in -selection clipboard"
        bind-key -T copy-mode C-g send-keys -X cancel

        # Paste from X clipboard
        bind C-y run "xclip -out -selection clipboard | tmux load-buffer - && tmux paste-buffer"

        # Popup scratch terminal (C-b g)
        bind g display-popup -E -w 80% -h 75% -x C -y C "urxvtc"

        # Resurrect: C-b C-s save / C-b C-r restore
        bind C-s run-shell "#{@resurrect-save-script-path} quiet"
        bind C-r run-shell "#{@resurrect-restore-script-path}"

        # Reload config
        bind r source-file ~/.config/tmux/tmux.conf \; display "reloaded"

        # Status bar
        set -g status-position bottom
        set -g status-style "fg=${theme.foreground},bg=${theme.background}"
        set -g window-status-current-style "fg=${theme.background},bg=${theme.active}"
        set -g status-left " [#S] "
        set -g status-right " %H:%M "
      '';
    };

    xsession.enable = true;
    xresources.properties = {
      "URxvt.font"      = "xft:${theme.font}:size=10";
      "URxvt.boldFont"  = "xft:${theme.font}:bold:size=10";
      "URxvt.italicFont" = "xft:${theme.font}:italic:size=10";

      "URxvt.foreground" = theme.foreground;
      "URxvt.background" = theme.background;

      "URxvt.color0"  = theme.color0;
      "URxvt.color1"  = theme.color1;
      "URxvt.color2"  = theme.color2;
      "URxvt.color3"  = theme.color3;
      "URxvt.color4"  = theme.color4;
      "URxvt.color5"  = theme.color5;
      "URxvt.color6"  = theme.color6;
      "URxvt.color7"  = theme.color7;
      "URxvt.color8"  = theme.color8;
      "URxvt.color9"  = theme.color9;
      "URxvt.color10" = theme.color10;
      "URxvt.color11" = theme.color11;
      "URxvt.color12" = theme.color12;
      "URxvt.color13" = theme.color13;
      "URxvt.color14" = theme.color14;
      "URxvt.color15" = theme.color15;

      "URxvt.cursorColor"        = theme.cursor;
      "URxvt.highlightColor"     = theme.highlight;
      "URxvt.highlightTextColor" = theme.highlightText;

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
        inputs.llm-agents.packages.${system}.coderabbit-cli
        
        (pkgs.writeShellScriptBin "browser"  "exec librewolf \"$@\"")
        (pkgs.writeShellScriptBin "editor"   "exec emacsclient -c -a emacs \"$@\"")
        (pkgs.writeShellScriptBin "terminal" "exec urxvtc \"$@\"")

        (pkgs.writeShellScriptBin "Ldef" "exec L def \"$@\"")
        (pkgs.writeShellScriptBin "Lrefs" "exec L refs \"$@\"")
        (pkgs.writeShellScriptBin "Lrn" "exec L rn \"$@\"")
        (pkgs.writeShellScriptBin "Lassist" "exec L assist \"$@\"")

           #pkgs.acme-lsp
           pkgs.pkg-config
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
      gtk4.theme = config.gtk.theme;
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
    
    home.file = assets.wallpapers 11 // assets.assets // {
      # ── Theme files (override static assets) ──────────────────────────────
      ".fvwm/config".text                = renderTemplate ../templates/fvwm3.conf.tmpl theme;
      ".config/dunst/dunstrc".text       = dunstrcFor theme;
      ".config/themes/dark.edn".text     = themeToEdn themes.dark;
      ".config/themes/light.edn".text    = themeToEdn themes.light;
      ".local/bin/theme.clj"             = { source = ../assets/scripts/theme.clj; executable = true; };

      # ── Edwood / acme-lsp ─────────────────────────────────────────────────
      ".config/acme-lsp/config.toml".text = ''
        FormatOnPut = true

        [Servers]

          [Servers.gopls]
          Command = ["gopls"]

          [Servers.pyright]
          Command = ["pyright-langserver", "--stdio"]

          [Servers.rust]
          Command = ["rust-analyzer"]

          [Servers.ts]
          Command = ["typescript-language-server", "--stdio"]

          [Servers.php]
          Command = ["phpactor", "language-server"]

        [[FilenameHandlers]]
        Pattern = "\\.go$"
        LanguageID = "go"
        ServerKey = "gopls"

        [[FilenameHandlers]]
        Pattern = "\\.py$"
        LanguageID = "python"
        ServerKey = "pyright"

        [[FilenameHandlers]]
        Pattern = "\\.rs$"
        LanguageID = "rust"
        ServerKey = "rust"

        [[FilenameHandlers]]
        Pattern = "\\.(ts|js)$"
        LanguageID = "typescript"
        ServerKey = "ts"

        [[FilenameHandlers]]
        Pattern = "\\.php$"
        LanguageID = "php"
        ServerKey = "php"
      '';

      # Launch edwood with fontsrv (X11→Plan9 font bridge) + acme-lsp
      ".local/bin/ew" = {
        executable = true;
        text = ''
          #!/usr/bin/env sh

          # Start fontsrv only if not already running
          if ! pgrep -x fontsrv >/dev/null 2>&1; then
            9 fontsrv &
            FSRV=$!
            trap 'kill "$FSRV" 2>/dev/null' EXIT INT TERM
            sleep 0.3
          fi

          # Remove stale acme socket — edwood panics if it already exists
          DISP="''${DISPLAY%%.*}"
          rm -f "/tmp/ns.$USER.$DISP/acme" 2>/dev/null

          # Launch edwood:
          #   -f  variable-width font  (proportional, for tag bars / labels)
          #   -F  fixed-width font     (monospace, for code bodies)
          # Font path format: /mnt/font/<Name>/<size>a/font  (a = antialiased)
          edwood \
            -f /mnt/font/DejaVuSans/13a/font \
            -F /mnt/font/DejaVuSansMono/13a/font \
            "$@" &
          EDWOOD_PID=$!

          # Wait for edwood to post its 9P server, then start acme-lsp
          sleep 0.5
          acme-lsp &

          wait "$EDWOOD_PID"
        '';
      };
      # ────────────────────────────────────────────────────────────────────
      ".kshrc".text = ''
  # Only interactive shells   
  [[ $- != *i* ]] && return   
  
  # === History ===
  HISTFILE="$HOME/.ksh_history"
  HISTSIZE=10000
  SAVEHIST=20000
  
  PS1='\033[38;5;67m$(hostname -s):$(pwd | sed "s|$HOME|~|")\033[0m $ '
  
  # === Aliases ===
  alias ll='ls -lh --color=auto'
  alias la='ls -lah --color=auto'
  alias gs='git status'
  alias gc='git commit'
  alias ga='git add'
  alias gl='git log --oneline --graph --decorate'
  alias browser='librewolf'
  alias editor='emacsclient -c -a emacs'
  alias terminal='urxvtc'
  alias em='emacsclient -nw'
  alias e='emacsclient -c -a emacs'
  alias ec="emacsclient -c > /dev/null'"
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

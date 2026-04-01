{ config, pkgs, lib, inputs, system, ... }:

let
  cli  = import ./cli.nix { inherit pkgs; isDarwin = true; };
  iac  = import ./development/infrastructure.nix { inherit pkgs; };
  programming-languages =
    import ./development/programming-languages.nix { inherit pkgs; isDarwin = true; };

  myEmacs =
    let
      emacsPkgSet = pkgs.emacsPackagesFor pkgs.emacs30;
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

in
{
  #### User (registers existing macOS user with home-manager) ##############

  users.users.wmb = {
    name = "wmb";
    home = "/Users/wmb";
  };

  #### Window manager + hotkeys ############################################

  services.yabai = {
    enable = true;
    extraConfig = ''
      # Float layout — closest to FVWM3 floating model
      yabai -m config layout float

      # Focus
      yabai -m config mouse_follows_focus off
      yabai -m config focus_follows_mouse  off

      # Mouse
      yabai -m config mouse_modifier       cmd
      yabai -m config mouse_action1        move
      yabai -m config mouse_action2        resize

      # Gaps (minimal)
      yabai -m config top_padding    4
      yabai -m config bottom_padding 4
      yabai -m config left_padding   4
      yabai -m config right_padding  4
      yabai -m config window_gap     4

      # Opacity
      yabai -m config window_opacity         off
      yabai -m config active_window_opacity  1.0
      yabai -m config normal_window_opacity  0.9
    '';
  };

  services.skhd = {
    enable = true;
    skhdConfig = ''
      # === Applications ===
      # Terminal
      cmd - return : open -na Ghostty

      # Emacs
      cmd - e : emacsclient -c

      # Launcher — cmd+space; disable Spotlight shortcut in System Settings first
      # (Raycast registers its own shortcut — configure it in Raycast prefs instead)

      # Lock screen
      shift + cmd - l : pmset displaysleepnow

      # === Window management ===
      # Toggle fullscreen
      shift + cmd - space : yabai -m window --toggle zoom-fullscreen

      # Cycle focus (cmd+tab handles app switching; this cycles within space)
      cmd - o : yabai -m window --focus next 2>/dev/null || yabai -m window --focus first

      # === Desktops / Spaces ===
      # Switch to space (create 5 spaces in System Settings > Desktop & Dock once)
      cmd - 1 : yabai -m space --focus 1
      cmd - 2 : yabai -m space --focus 2
      cmd - 3 : yabai -m space --focus 3
      cmd - 4 : yabai -m space --focus 4
      cmd - 5 : yabai -m space --focus 5

      # Move window to space and follow
      shift + cmd - 1 : yabai -m window --space 1 && yabai -m space --focus 1
      shift + cmd - 2 : yabai -m window --space 2 && yabai -m space --focus 2
      shift + cmd - 3 : yabai -m window --space 3 && yabai -m space --focus 3
      shift + cmd - 4 : yabai -m window --space 4 && yabai -m space --focus 4
      shift + cmd - 5 : yabai -m window --space 5 && yabai -m space --focus 5

      # === Window movement (Super+Alt+f/b/n/p → cmd+alt+f/b/n/p) ===
      cmd + alt - f : yabai -m window --move rel:100:0
      cmd + alt - b : yabai -m window --move rel:-100:0
      cmd + alt - n : yabai -m window --move rel:0:100
      cmd + alt - p : yabai -m window --move rel:0:-100

      # === Window resize (Super+Ctrl+Alt+f/b/n/p → cmd+ctrl+alt+f/b/n/p) ===
      cmd + ctrl + alt - f : yabai -m window --resize right:100:0; yabai -m window --resize left:-100:0
      cmd + ctrl + alt - b : yabai -m window --resize left:100:0;  yabai -m window --resize right:-100:0
      cmd + ctrl + alt - n : yabai -m window --resize bottom:0:100
      cmd + ctrl + alt - p : yabai -m window --resize top:0:-100
    '';
  };

  #### Home Manager ########################################################

  home-manager.users.wmb = { pkgs, ... }: {
    home.stateVersion = "25.05";
    home.username = "wmb";
    home.homeDirectory = "/Users/wmb";

    nixpkgs.config.allowUnfree = true;

    programs.home-manager.enable = true;

    home.packages =
      cli
      ++ programming-languages
      ++ iac
      ++ [ myEmacs

        (pkgs.writeShellScriptBin "Ldef" "exec L def \"$@\"")
        (pkgs.writeShellScriptBin "Lrefs" "exec L refs \"$@\"")
        (pkgs.writeShellScriptBin "Lrn" "exec L rn \"$@\"")
        (pkgs.writeShellScriptBin "Lassist" "exec L assist \"$@\"")

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

    # === Session Variables ===
    home.sessionVariables = {
      EDITOR = "emacsclient -c -a emacs";
      VISUAL = "$EDITOR";
      PAGER = "less -R";
      MANPAGER = "sh -c 'col -bx | bat -l man -p'";
      ENV = "$HOME/.kshrc";
    };

    # === Session Path ===
    home.sessionPath = [
      "$HOME/.local/bin"
      "$HOME/go/bin"
      "$HOME/.cargo/bin"
    ];

    home.file = {
      ".emacs.d/init.el".source = ../assets/init.el;
      ".emacs.d/lisp/packages.el".source = ../assets/packages.el;
      ".config/ghostty/config".source = ../assets/ghostty;

      ".kshrc".text = ''
# Only interactive shells
[[ $- != *i* ]] && return

# === History ===
HISTFILE="$HOME/.ksh_history"
HISTSIZE=10000
SAVEHIST=20000

PS1='\033[38;5;67m$(pwd | sed "s|$HOME|~|")\033[0m $ '

# === Aliases ===
alias ll='ls -lhG'
alias la='ls -lahG'
alias gs='git status'
alias gc='git commit'
alias ga='git add'
alias gl='git log --oneline --graph --decorate'
alias em='emacsclient -nw'
alias e='emacsclient -c -a emacs'
alias ns='nix-shell'
alias nb='nix build'
alias nf='nix flake show'
alias nh='nix develop'

# === fzf ===
if command -v fzf >/dev/null 2>&1; then
  export FZF_DEFAULT_COMMAND='rg --files --hidden --follow --glob "!.git/*"'
  export FZF_CTRL_T_COMMAND="$FZF_DEFAULT_COMMAND"
  export FZF_DEFAULT_OPTS="--height 40% --layout=reverse --border"
fi

# === Direnv ===
if command -v direnv >/dev/null 2>&1; then
  eval "$(direnv hook ksh)"
fi

# === Starship ===
if command -v starship >/dev/null 2>&1; then
  eval "$(starship init ksh)"
fi
'';
    };
  };
}

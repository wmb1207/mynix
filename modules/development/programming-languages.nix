{ pkgs, isDarwin ? false }:

with pkgs;
let
  common = [
    janet
    ocaml
    dune_3
    opam
    ocamlPackages.ocamlformat
    ocamlPackages.merlin
    ocamlPackages.utop
    ocamlPackages.lsp
    ocamlPackages.ocaml-lsp

    php84
    nodejs
    python313
    go
    elixir
    erlang
    typescript
    yarn
    pipx
    hex

    scala
    metals
    scalafmt

    tree-sitter-grammars.tree-sitter-php
    gopls
    go-outline
    gopkgs
    gotools
    delve

    pyright
    rust-analyzer
    phpactor
  ];

  linux = [
    crystal
    crystalline
    shards
  ];

in
  common ++ (if isDarwin then [] else linux)

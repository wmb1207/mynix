{ pkgs }:

with pkgs; [
  ocaml
  dune_3  # dune build system
  opam    # package manager (optional)
  #utop    # nice REPL
  ocamlPackages.ocamlformat  # formatter
  ocamlPackages.merlin  # formatter
  ocamlPackages.utop
  ocamlPackages.lsp
  ocamlPackages.ocaml-lsp
  
  #merlin  # editor support (LSP helper)
  php84
  nodejs
  python313
  #ruby
  go
  elixir
  erlang
  typescript
  yarn
  pipx
  hex
  crystal
  crystalline
  shards

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
]

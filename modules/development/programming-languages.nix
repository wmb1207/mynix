{ pkgs }:

with pkgs; [
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

  tree-sitter-grammars.tree-sitter-php
  gopls
  go-outline
  gopkgs
  gotools
  delve 
]

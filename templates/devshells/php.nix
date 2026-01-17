{
  description = "Basic PHP dev shell";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/master";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};

        # You can specify the version with the right package
        php = pkgs.php.buildEnv {
          extensions = ({ enabled, all }: (with all; enabled ++ [
            redis
            xdebug
            pdo
            pdo_mysql
            pdo_pgsql
            pgsql
            curl
            mbstring
            openssl
            tokenizer
            zip
            fileinfo
            bcmath
            dom
            intl
          ]));
          extraConfig = ''
            memory_limit = -1
            xdebug.mode = debug,develop
            xdebug.start_with_request = yes
            xdebug.client_host = 127.0.0.1
            xdebug.client_port = 9003
          '';
        };

      in {
        devShell = pkgs.mkShell {
          name = "dev-shell";
          buildInputs = with pkgs; [
            php
            php.packages.composer
            nodejs
            openssl
            curl
            git
          ];

          shellHook = ''
            export PHP_IDE_CONFIG="serverName=laravel"
            export XDEBUG_MODE=debug
            export XDEBUG_CONFIG="client_host=127.0.0.1 client_port=9003"

            echo "Laravel DevShell (PHP 8.2 + Node 20 + Xdebug) loaded."
          '';
        };
      });
}

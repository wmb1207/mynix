{
  description = "42 mate teleport flake";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
  };

  outputs = { self, nixpkgs }:
  let
    systems = [ "x86_64-linux" "aarch64-linux" "x86_64-darwin" ];
  in
  {

    packages = nixpkgs.lib.genAttrs systems (system:
      let pkgs = import nixpkgs { inherit system; };
      in pkgs.stdenv.mkDerivation {
        name = "teleport-installer";
        src = ./teleport-install.sh;

        buildInputs = [ pkgs.bash ];

        installPhase = ''
          mkdir -p $out/bin
          cp $src $out/bin/teleport-install
          chmod +x $out/bin/teleport-install
        '';
      }
    );

    apps = nixpkgs.lib.genAttrs systems (system: {
      default = {
        type = "app";
        program = "${self.packages.${system}}/bin/teleport-install";
      };
    });

    devShells = nixpkgs.lib.genAttrs systems (system:
      let pkgs = import nixpkgs { inherit system; };
      in {
        default = pkgs.mkShell {
          buildInputs = [
            pkgs.bash
            pkgs.curl
            pkgs.wget
            pkgs.coreutils
            pkgs.gnused
          ];
        };
      }
    );
  };
}

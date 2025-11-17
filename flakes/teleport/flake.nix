{
  description = "Teleport v15 Mate Nixos";
  
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
  };
  
  outputs = { self, nixpkgs }: let
    systems = [
      "x86_64-linux"
      "aarch64-linux"
      "x86_64-darwin"
    ];
  in {
    packages = nixpkgs.lib.genAttrs systems (system:
      let
        pkgs = import nixpkgs { inherit system; };
        version = "15.5.4";
      in {
        teleport = pkgs.buildGoModule rec {
          pname = "teleport";
          inherit version;
          
          src = pkgs.fetchFromGitHub {
            owner = "gravitational";
            repo = "teleport";
            rev  = "v${version}";
            hash = "sha256-iYSh8cpdumeJ480dPBfsuE9zDe5WFBCZHo+N3I6sIhM=";
          };
          
          vendorHash = "sha256-xeOnxXZ1VHdArpYC8EIkA9TxIT14SAJVNRAHfOxrlds=";
          subPackages = [
            "tool/tsh"
            "tool/teleport"
            "tool/tctl"
            "tool/tbot"
          ];
          
          tags = [ "libfido2" ];
          
          nativeBuildInputs = [ 
            pkgs.pkg-config 
            pkgs.go_1_25
          ];
          
          buildInputs = [
            pkgs.openssl
            pkgs.libfido2
          ];
          
          preBuild = ''
            export GOROOT="${pkgs.go_1_25}/share/go"
          '';
          
          ldflags = [
            "-s"
            "-w"
            "-X github.com/gravitational/teleport/lib/defaults.TeleportVersion=${version}"
          ];
          
          doCheck = false;
          
          meta = with pkgs.lib; {
            description = "Teleport v${version} - Certificate authority and access plane for SSH, Kubernetes, web apps, and databases";
            homepage    = "https://goteleport.com/";
            license     = licenses.agpl3Plus;
            mainProgram = "tsh";
            platforms   = platforms.unix;
          };
        };
        
        default = self.packages.${system}.teleport;
      }
    );
    
    apps = nixpkgs.lib.genAttrs systems (system: {
      default = {
        type    = "app";
        program = "${self.packages.${system}.teleport}/bin/tsh";
      };
      tsh = {
        type    = "app";
        program = "${self.packages.${system}.teleport}/bin/tsh";
      };
      teleport = {
        type    = "app";
        program = "${self.packages.${system}.teleport}/bin/teleport";
      };
      tctl = {
        type    = "app";
        program = "${self.packages.${system}.teleport}/bin/tctl";
      };
      tbot = {
        type    = "app";
        program = "${self.packages.${system}.teleport}/bin/tbot";
      };
    });
    
    nixosModules.default = { config, pkgs, lib, system, ... }: {
      options.services.teleport-client = {
        enable = lib.mkEnableOption "Teleport client (tsh)";
      };
      
      config = lib.mkIf config.services.teleport-client.enable {
        environment.systemPackages = [
          self.packages.${system}.teleport
        ];
      };
    };
    
    homeManagerModules.default = { config, pkgs, lib, system, ... }: {
      options.programs.teleport-client = {
        enable = lib.mkEnableOption "Teleport client (tsh)";
      };
      
      config = lib.mkIf config.programs.teleport-client.enable {
        home.packages = [
          self.packages.${system}.teleport
        ];
      };
    };
  };
}

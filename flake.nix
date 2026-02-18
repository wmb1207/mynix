{
  description = "wmb NIX setup";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

    teleport-installer.url = "path:./flakes/teleport";

    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = inputs @ { self, nixpkgs, home-manager, teleport-installer, ... }:
    let
      system = "x86_64-linux";

      baseModules = [
        home-manager.nixosModules.default
      ];

      myOverlays = [
        (self: super: {
          libsForQt5 = super.libsForQt5 // {
            "fcitx5-with-addons" = null;
          };
        })
      ];

      pkgsFor = system: import nixpkgs {
        inherit system;
        config.allowUnfree = true;
        overlays = myOverlays;
      };

    in {
      nixosConfigurations = {

        default = nixpkgs.lib.nixosSystem {
          inherit system;
          pkgs = pkgsFor system;
          modules = baseModules;
        };

        nixos = nixpkgs.lib.nixosSystem {
          inherit system;
          pkgs = pkgsFor system;
          modules = baseModules ++ [
            ./hosts/desktop/configuration.nix
            ./modules/user-wmb.nix
            teleport-installer.nixosModules.default
          ];
          specialArgs = {
            inherit inputs system teleport-installer;
          };
        };

        rog = nixpkgs.lib.nixosSystem {
          inherit system;
          pkgs = pkgsFor system;
          modules = baseModules ++ [
            ./hosts/asus/configuration.nix
            ./modules/user-wmb.nix
            ./modules/laptop-keyboard.nix
          ];
          specialArgs = {
            inherit inputs system teleport-installer;
          };
        };

        latitude = nixpkgs.lib.nixosSystem {
          inherit system;
          pkgs = pkgsFor system;
          modules = baseModules ++ [
            ./hosts/latitude/configuration.nix
            ./modules/user-wmb.nix
            ./modules/laptop-keyboard.nix
            teleport-installer.nixosModules.default
            {
              environment.systemPackages = [
                teleport-installer.packages.${system}.teleport
              ];
            }
          ];
          specialArgs = {
            inherit inputs system teleport-installer;
          };
        };

        vm = nixpkgs.lib.nixosSystem {
          inherit system;
          pkgs = pkgsFor system;
          modules = baseModules ++ [
            ./hosts/vm/configuration.nix
            ./modules/user-wmb.nix
            ./modules/laptop-keyboard.nix
          ];
          specialArgs = {
            inherit inputs system teleport-installer;
          };
        };

        asahibook = nixpkgs.lib.nixosSystem {
          system = "aarch64-linux";
          pkgs = pkgsFor "aarch64-linux";
          modules = baseModules ++ [
            ./hosts/asahi-book/configuration.nix
            ./modules/user-wmb.nix
            ./modules/laptop-keyboard.nix
          ];
          specialArgs = {
            inherit inputs teleport-installer;
            system = "aarch64-linux";
          };
        };
      };

      homeConfigurations = {
        wmb = home-manager.lib.homeManagerConfiguration {
          pkgs = pkgsFor system;
          modules = [
            ./modules/user-wmb.nix
            {
              home.username = "wmb";
              home.homeDirectory = "/home/wmb";
              home.stateVersion = "23.05";
            }
            {
              home.packages = [
                teleport-installer.packages.${system}.default
              ];
            }
          ];
        };
      };
    };
}

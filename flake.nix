{
  description = "wmb NIX setup";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
     
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { self, nixpkgs, home-manager, ... }@inputs:
    let
      system = "x86_64-linux";
      baseModules = [
        inputs.home-manager.nixosModules.default
      ];

      myOverlays = [
        (self: super: {
          libsForQt5 = super.libsForQt5 // {
            "fcitx5-with-addons" = null;
          };
        })
      ];

    in {
      nixosConfigurations = {

        default = nixpkgs.lib.nixosSystem {
          system = "x86_64-linux";
          pkgs = nixpkgs.legacyPackages.x86_64-linux;
          modules = baseModules;
        };

        nixos = nixpkgs.lib.nixosSystem {
          system = "x86_64-linux";
          pkgs = nixpkgs.legacyPackages.x86_64-linux // {
            overlays = myOverlays;
          };
          modules = baseModules ++ [
            ./hosts/desktop/configuration.nix
            ./modules/user-wmb.nix
          ];
          specialArgs = { inherit inputs system; };
        };

        rog = nixpkgs.lib.nixosSystem {
          system = "x86_64-linux";
          pkgs = nixpkgs.legacyPackages.x86_64-linux // {
            overlays = myOverlays;
          };
          modules = baseModules ++ [
            ./hosts/asus/configuration.nix
            ./modules/user-wmb.nix
            ./modules/laptop-keyboard.nix
          ];
          specialArgs = { inherit inputs system; };
        };

        latitude = nixpkgs.lib.nixosSystem {
          system = "x86_64-linux";
          pkgs = import nixpkgs {
            system = "x86_64-linux";
            config.allowUnfree = true;  # <-- pass it here, not in configuration.nix
          };
          modules = baseModules ++ [
            ./hosts/latitude/configuration.nix
            ./modules/user-wmb.nix
            ./modules/laptop-keyboard.nix
          ];
          specialArgs = { system = "x86_64-linux"; inherit inputs; };
        };


        asahibook = nixpkgs.lib.nixosSystem {
          system = "aarch64-linux";
          pkgs = nixpkgs.legacyPackages.aarch64-linux // {
            overlays = myOverlays;
          };
          modules = baseModules ++ [
            ./hosts/asahi-book/configuration.nix
            ./modules/user-wmb.nix
            ./modules/laptop-keyboard.nix
          ];
          specialArgs = { system = "aarch64-linux"; inherit inputs; };
        };
      };

      homeConfigurations = {
        wmb = home-manager.lib.homeManagerConfiguration {
          pkgs = nixpkgs.legacyPackages.x86_64-linux // { overlays = myOverlays; };
          modules = [
            ./modules/user-wmb.nix
            {
              home.username = "wmb";
              home.homeDirectory = "/home/wmb";
              home.stateVersion = "23.05";
            }
          ];
        };
      };
    };
}


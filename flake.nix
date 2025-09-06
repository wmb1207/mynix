{
  description = "wmb NIX setup";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs?ref=nixos-unstable";
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { self, nixpkgs, home-manager, ... }@inputs:
    let
      baseModules = [
        inputs.home-manager.nixosModules.default
      ];
      system = "x86_64-linux";  
      pkgs = import nixpkgs { inherit system; config.allowUnfree = true; };
    in {
      nixosConfigurations = {
        default = nixpkgs.lib.nixosSystem {
          system = "x86_64-linux";
          pkgs = nixpkgs.legacyPackages.x86_64-linux;
          modules = baseModules;
        };

        nixos = nixpkgs.lib.nixosSystem {
          system = "x86_64-linux";
          pkgs = import nixpkgs {
            system = "x86_64-linux";
            config.allowUnfree = true;
            #legacyPackages.x86_64-linhux;
          };
          modules = baseModules ++ [
            ./hosts/desktop/configuration.nix
            ./modules/user-wmb.nix
          ];
          specialArgs = {          system = "x86_64-linux"; inherit inputs; };
        };

        rog = nixpkgs.lib.nixosSystem {
          system = "x86_64-linux";
	        pkgs = import nixpkgs {
            system = "x86_64-linux";
            config.allowUnfree = true;
            #nixpkgs.legacyPackages.aarch64-linux;
          };
	  
          modules = baseModules ++ [
            ./hosts/asus/configuration.nix
            ./modules/user-wmb.nix
            ./modules/laptop-keyboard.nix
          ];
          specialArgs = {          system = "x86_64-linux"; inherit inputs; };
        };

        latitude = nixpkgs.lib.nixosSystem {
          system = "x86_64-linux";

			pkgs = import nixpkgs {
            	    system = "x86_64-linux";
            config.allowUnfree = true;
            #legacyPackages.x86_64-linhux;
          };
          modules = baseModules ++ [
            ./hosts/latitude/configuration.nix
            ./modules/user-wmb.nix
            ./modules/laptop-keyboard.nix
          ];
          specialArgs = {          system = "x86_64-linux"; inherit inputs; };
        };


        asahibook = nixpkgs.lib.nixosSystem {
          system = "aarch64-linux";
          pkgs = import nixpkgs {
            system = "aarch64-linux";
            config.allowUnfree = true;
            #nixpkgs.legacyPackages.aarch64-linux;
          };
          modules = baseModules ++ [
            ./hosts/asahi-book/configuration.nix
            ./modules/user-wmb.nix
            ./modules/laptop-keyboard.nix
          ];
          specialArgs = {          system = "aarch64-linux"; inherit inputs; };
        };
      };
#      homeConfigurations = {
#   wmb = home-manager.lib.homeManagerConfiguration {
#     pkgs = pkgs;
#     modules = [
#       ./modules/user-wmb.nix
#       {
#         # User-specific settings go inside a module
#         home.username = "wmb";
#         home.homeDirectory = "/home/wmb";

#         # You can also set stateVersion here if needed
#         home.stateVersion = "23.05"; # or whatever version you want
#       }
#     ];
#   };
# };
    };
}

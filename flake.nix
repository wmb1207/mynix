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
      system = "x86_64-linux";
      pkgs = nixpkgs.legacyPackages.${system};
      baseModules = [
        inputs.home-manager.nixosModules.default
      ];
    in {
      nixosConfigurations = {
        default = nixpkgs.lib.nixosSystem {
          system = system;
          modules = baseModules;
        };
        nixos = nixpkgs.lib.nixosSystem {
          system = system;
          modules = baseModules ++ [
            ./hosts/desktop/configuration.nix
            ./modules/user-wmb.nix
          ];
          # Pass `inputs` directly to the module
          specialArgs = { inherit inputs; }; 
        };
        rog = nixpkgs.lib.nixosSystem {
          system = system;
          modules = baseModules ++ [
            ./hosts/asus/configuration.nix
            ./modules/user-wmb.nix
	    ./modules/laptop-keyboard.nix
          ];
          # Pass `inputs` directly to the module
          specialArgs = { inherit inputs; };
        };
      };
    };
}

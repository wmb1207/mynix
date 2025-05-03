{
  description = "wmb NIX setup";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs?ref=nixos-unstable";
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { self, nixpkgs, ...}@inputs: 
    let
      system = "x86_64-linux";
      pkgs = nixpkgs.legacyPackages.${system};
      baseModules =[
        ./configuration.nix
        inputs.home-manager.nixosModules.default
      ];
    in {
      nixosConfigurations = {
        default = nixpkgs.lib.nixosSystem {
          extraSpecialArgs = {inherit inputs;};
          modules = baseModules;
        };
        nixos = nixpkgs.lib.nixosSystem {
          modules = baseModules ++ [
            ./modules/user-wmb.nix
          ];
  	    };
      };
    };
}

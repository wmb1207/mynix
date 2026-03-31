{
  description = "wmb NIX setup";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

    teleport-installer.url = "path:./flakes/teleport";

    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    nix-darwin = {
      url = "github:LnL7/nix-darwin";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = inputs @ { self, nixpkgs, home-manager, nix-darwin, teleport-installer, ... }:
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
        pi = nixpkgs.lib.nixosSystem {
          system = "aarch64-linux";
          pkgs = pkgsFor "aarch64-linux";
          modules = [
            "${nixpkgs}/nixos/modules/installer/sd-card/sd-image-aarch64-installer.nix"
            ./hosts/pi/configuration.nix
            {
              sdImage.compressImage = false;
            }
          ];
        };

        workstationiso = nixpkgs.lib.nixosSystem {
          inherit system;
          pkgs = pkgsFor system;

          modules = baseModules ++ [
            "${nixpkgs}/nixos/modules/installer/cd-dvd/installation-cd-minimal.nix"
            ./modules/iso-base.nix
            ./modules/iso-gui.nix
            ./modules/user-wmb.nix
            ./modules/laptop-keyboard.nix
            teleport-installer.nixosModules.default

            {
              environment.systemPackages = [
                teleport-installer.packages.${system}.teleport
              ];

              # usually good for install/live media
              services.openssh.enable = true;

              # often needed to avoid bootloader/disk config conflicts
              boot.loader.grub.enable = false;
              virtualisation.docker.enable = true; 
            }
          ];
          specialArgs = {
            inherit inputs system teleport-installer;
          };
        };

        genericlaptop = nixpkgs.lib.nixosSystem {
          inherit system;
          pkgs = pkgsFor system;
          modules = baseModules ++ [
            ./hosts/genericlaptop/hardware-configuration.nix
            ./hosts/genericlaptop/configuration.nix
            ./modules/user-wmb.nix
            ./modules/laptop-keyboard.nix
            {
              # Bootloader configuration
              boot.loader.systemd-boot.enable = true;
              boot.loader.efi.canTouchEfiVariables = true;

              virtualisation.docker.enable = true;
            }
          ];
          specialArgs = {
            inherit inputs system teleport-installer;
          };
        };

        genericdesktop = nixpkgs.lib.nixosSystem {
          inherit system;
          pkgs = pkgsFor system;
          modules = baseModules ++ [
            ./hosts/genericdesktop/hardware-configuration.nix
            ./hosts/genericdesktop/configuration.nix
            ./modules/user-wmb.nix
            {
              # Bootloader configuration
              boot.loader.systemd-boot.enable = true;
              boot.loader.efi.canTouchEfiVariables = true;

              virtualisation.docker.enable = true;
            }
          ];
          specialArgs = {
            inherit inputs system teleport-installer;
          };
        };                      # 

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

        asahimini = nixpkgs.lib.nixosSystem {
          system = "aarch64-linux";
          pkgs = pkgsFor "aarch64-linux";
          modules = baseModules ++ [
            ./hosts/asahi-mini/configuration.nix
            ./modules/user-wmb.nix
          ];
          specialArgs = {
            inherit inputs teleport-installer;
            system = "aarch64-linux";
          };
        };

        postgres = nixpkgs.lib.nixosSystem {
          inherit system;
          pkgs = pkgsFor system;
          modules = baseModules ++ [
            ./hosts/postgres/configuration.nix

            ({ config, lib, pkgs, modulesPath, ... }: {
              imports = [
                "${modulesPath}/profiles/qemu-guest.nix"
              ];
              
              fileSystems."/" = {
                device = "/dev/disk/by-label/nixos";
                fsType = "ext4";
                autoResize = true;
              };
              
              boot.loader.grub.enable = true;
              boot.loader.grub.device = "/dev/vda";
              
              system.build.qcow2 = import "${modulesPath}/../lib/make-disk-image.nix" {
                inherit lib config pkgs;
                format = "qcow2";
                diskSize = 20 * 1024;
                partitionTableType = "hybrid";
              };
            })
          ];
          specialArgs = {
            inherit inputs system teleport-installer;
          };
        };
        
        mongo = nixpkgs.lib.nixosSystem {
          inherit system;
          pkgs = pkgsFor system;
          modules = [
            ./hosts/mongo/configuration.nix
          ];
          specialArgs = {
            inherit inputs system teleport-installer;
          };
        };

        tailscalevm = nixpkgs.lib.nixosSystem {
          inherit system;
          pkgs = pkgsFor system;
          modules = [
            ./hosts/tailscale/configuration.nix
          ];
        };
      };

      darwinConfigurations = {
        macbook = nix-darwin.lib.darwinSystem {
          system = "aarch64-darwin";
          modules = [
            home-manager.darwinModules.home-manager
            ./hosts/macbook/configuration.nix
            ./modules/user-wmb-darwin.nix
          ];
          specialArgs = {
            inherit inputs;
            system = "aarch64-darwin";
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

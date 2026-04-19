{
  description = "wmb NIX setup";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

    teleport-installer.url = "path:./flakes/teleport";

    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    llm-agents = {
      url = "github:numtide/llm-agents.nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = inputs @ { self, nixpkgs, home-manager, teleport-installer, llm-agents, ... }:
    let
      system = "x86_64-linux";

      baseModules = [
        home-manager.nixosModules.default
        ./modules/wmb-arpa-ca.nix
      ];

      myOverlays = [
        (self: super: {
          libsForQt5 = super.libsForQt5 // {
            "fcitx5-with-addons" = null;
          };
        })
        # cptofs (LKL tool used by make-disk-image.nix) hardcodes mem=100M which is
        # too small for large NixOS closures (e.g. podman). Binary-patch it to 512M.
        # Note: lkl has outputs=["dev" "lib" "out"], so `super.lkl` resolves to the
        # dev output; we must use `super.lkl.out` explicitly to get the binaries.
        (self: super: {
          lkl = (super.runCommand "lkl-mempatched" {} ''
            mkdir -p $out
            cp -a ${super.lkl.out}/. $out/
            chmod -R +w $out
            sed -i 's/mem=100M/mem=512M/g' $out/bin/cptofs
          '') // { inherit (super.lkl) lib dev; };
        })
      ];

      pkgsFor = system: import nixpkgs {
        inherit system;
        config.allowUnfree = true;
        overlays = myOverlays;
      };

      sshKeys =
        if builtins.pathExists /home/wmb/.ssh/id_rsa.pub
        then [ (builtins.readFile /home/wmb/.ssh/id_rsa.pub) ]
        else [];

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

        nginx = nixpkgs.lib.nixosSystem {
          inherit system;
          pkgs = pkgsFor system;
          specialArgs = { inherit sshKeys; };
          modules = [
            ./hosts/nginx/configuration.nix

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
                diskSize = 10 * 1024;  # 10GB
                partitionTableType = "hybrid";
              };
            })
          ];
        };

        coredns = nixpkgs.lib.nixosSystem {
          inherit system;
          pkgs = pkgsFor system;
          specialArgs = { inherit sshKeys; };
          modules = [
            ./hosts/coredns/configuration.nix

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
                diskSize = 5 * 1024;  # 5GB
                partitionTableType = "hybrid";
              };
            })
          ];
        };

        redis = nixpkgs.lib.nixosSystem {
          inherit system;
          pkgs = pkgsFor system;
          modules = [
            ./hosts/redis/configuration.nix

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
                diskSize = 10 * 1024;  # 10GB
                partitionTableType = "hybrid";
              };
            })
          ];
        };

        podman-server = nixpkgs.lib.nixosSystem {
          inherit system;
          pkgs = pkgsFor system;
          specialArgs = { inherit sshKeys; };
          modules = [
            ./hosts/podman-server/configuration.nix

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
                diskSize = 40 * 1024;  # 40GB
                memSize = 2048;
                partitionTableType = "hybrid";
              };
            })
          ];
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

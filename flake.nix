{
  description = "wmb NIX setup";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

    crystal-greeter = {
      url = "github:wmb1207/greeter";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    studiowmb = {
      url = "git+https://git.studiowmb.com/studiowmb/studiowmb";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    teleport-installer.url = "path:./flakes/teleport";

    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    llm-agents = {
      url = "github:numtide/llm-agents.nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    wmbtooling = {
      url = "path:/home/wmb/dev/crystal/wmbtooling";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    disko = {
      url = "github:nix-community/disko";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = inputs @ { self, nixpkgs, home-manager, teleport-installer, llm-agents, wmbtooling, crystal-greeter, studiowmb, disko, ... }:
    let
      system = "x86_64-linux";
      user      = let
                    nixosUser = builtins.getEnv "NIXOS_USER";
                    envUser   = builtins.getEnv "USER";
                  in if nixosUser != "" then nixosUser
                     else if envUser != "" && envUser != "root" then envUser
                     else "wmb";
      homeDir = let
                  h = builtins.getEnv "HOME";
                  nixosUser = builtins.getEnv "NIXOS_USER";
                in if nixosUser != "" || h == "" || h == "/root" then "/home/${user}" else h;
      darkTheme = let t = builtins.getEnv "NIXOS_THEME"; in if t == "" then "wilson" else t;
      lightTheme = let t = builtins.getEnv "NIXOS_LIGHT_THEME"; in if t == "" then "doric-oak" else t;

      repoDir   = builtins.getEnv "PWD";
      localNix  = let p = "${repoDir}/modules/local.nix";
                  in if repoDir != "" && builtins.pathExists p then p else null;
      installSecrets = let p = "${repoDir}/modules/install-secrets.nix";
                       in if repoDir != "" && builtins.pathExists p then p else null;

      baseModules = [
        home-manager.nixosModules.default
        ./modules/wmb-arpa-ca.nix
      ] ++ (if installSecrets != null then [ installSecrets ] else []);

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
        if builtins.pathExists "${homeDir}/.ssh/id_rsa.pub"
        then [ (builtins.readFile "${homeDir}/.ssh/id_rsa.pub") ]
        else [];

      hoppscotchEnv =
        if builtins.pathExists ./hosts/podman-server/hoppscotch.env
        then builtins.readFile ./hosts/podman-server/hoppscotch.env
        else "";

      cloudflaredToken =
        let
          candidates =
            if repoDir == "" then []
            else [
              "${repoDir}/hosts/nginx/cloudflared-token.env"
              "${repoDir}/cloudflared-token.env"
            ];
          existing = builtins.filter builtins.pathExists candidates;
        in if existing != []
           then builtins.readFile (builtins.head existing)
           else if builtins.pathExists ./hosts/nginx/cloudflared-token.env
           then builtins.readFile ./hosts/nginx/cloudflared-token.env
           else "";

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
            ./modules/user.nix
            ./modules/greeter.nix
            ./modules/laptop-keyboard.nix
            teleport-installer.nixosModules.default

            {
              environment.systemPackages = [
                teleport-installer.packages.${system}.teleport
                disko.packages.${system}.disko
              ];

              services.openssh.enable = true;
              boot.loader.grub.enable = false;
            }
          ];
          specialArgs = {
            inherit inputs system teleport-installer crystal-greeter user darkTheme lightTheme localNix;
            userModuleMode = "nixos";
          };
        };

        genericlaptop = nixpkgs.lib.nixosSystem {
          inherit system;
          pkgs = pkgsFor system;
          modules = baseModules ++ [
            ./hosts/genericlaptop/hardware-configuration.nix
            ./hosts/genericlaptop/configuration.nix
            ./modules/user.nix
            ./modules/laptop-keyboard.nix
            {
              boot.loader.systemd-boot.enable = true;
              boot.loader.efi.canTouchEfiVariables = true;
              virtualisation.docker.enable = true;
            }
          ];
          specialArgs = {
            inherit inputs system teleport-installer user darkTheme lightTheme localNix;
            userModuleMode = "nixos";
          };
        };

        genericdesktop = nixpkgs.lib.nixosSystem {
          inherit system;
          pkgs = pkgsFor system;
          modules = baseModules ++ [
            ./hosts/genericdesktop/hardware-configuration.nix
            ./hosts/genericdesktop/configuration.nix
            ./modules/user.nix
            {
              boot.loader.systemd-boot.enable = true;
              boot.loader.efi.canTouchEfiVariables = true;
              virtualisation.docker.enable = true;
            }
          ];
          specialArgs = {
            inherit inputs system teleport-installer user darkTheme lightTheme localNix;
            userModuleMode = "nixos";
          };
        };

        nixos = nixpkgs.lib.nixosSystem {
          inherit system;
          pkgs = pkgsFor system;
          modules = baseModules ++ [
            ./hosts/desktop/configuration.nix
            ./modules/user.nix
            ./modules/greeter.nix
            teleport-installer.nixosModules.default
          ];
          specialArgs = {
            inherit inputs system teleport-installer crystal-greeter user darkTheme lightTheme localNix;
            userModuleMode = "nixos";
          };
        };

        rog = nixpkgs.lib.nixosSystem {
          inherit system;
          pkgs = pkgsFor system;
          modules = baseModules ++ [
            ./hosts/asus/configuration.nix
            ./modules/user.nix
            ./modules/laptop-keyboard.nix
            ./modules/greeter.nix
          ];
          specialArgs = {
            inherit inputs system teleport-installer crystal-greeter user darkTheme lightTheme localNix;
            userModuleMode = "nixos";
          };
        };

        latitude = nixpkgs.lib.nixosSystem {
          inherit system;
          pkgs = pkgsFor system;
          modules = baseModules ++ [
            ./hosts/latitude/configuration.nix
            ./modules/user.nix
            ./modules/laptop-keyboard.nix
            ./modules/greeter.nix
            teleport-installer.nixosModules.default
            {
              environment.systemPackages = [
                teleport-installer.packages.${system}.teleport
              ];
            }
          ];
          specialArgs = {
            inherit inputs system teleport-installer crystal-greeter user darkTheme lightTheme localNix;
            userModuleMode = "nixos";
          };
        };

        vm = nixpkgs.lib.nixosSystem {
          inherit system;
          pkgs = pkgsFor system;
          modules = baseModules ++ [
            ./hosts/vm/configuration.nix
            ./modules/user.nix
          ];
          specialArgs = {
            inherit inputs system teleport-installer user darkTheme lightTheme localNix;
            userModuleMode = "nixos";
          };
        };

        asahibook = nixpkgs.lib.nixosSystem {
          system = "aarch64-linux";
          pkgs = pkgsFor "aarch64-linux";
          modules = baseModules ++ [
            ./hosts/asahi-book/configuration.nix
            ./modules/user.nix
            ./modules/laptop-keyboard.nix
          ];
          specialArgs = {
            inherit inputs teleport-installer user darkTheme lightTheme localNix;
            userModuleMode = "nixos";
            system = "aarch64-linux";
          };
        };

        asahimini = nixpkgs.lib.nixosSystem {
          system = "aarch64-linux";
          pkgs = pkgsFor "aarch64-linux";
          modules = baseModules ++ [
            ./hosts/asahi-mini/configuration.nix
            ./modules/user.nix
          ];
          specialArgs = {
            inherit inputs teleport-installer user darkTheme lightTheme localNix;
            userModuleMode = "nixos";
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
          specialArgs = {
            inherit sshKeys cloudflaredToken;
            studiowmbPkg = studiowmb.packages.${system}.default;
          };
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
          specialArgs = { inherit sshKeys hoppscotchEnv; };
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

      diskoConfigurations =
        let
          genericWorkstationDisk = { lib, diskDevice ? "/dev/sda", ... }:
            import ./hosts/disk-layouts/gpt-efi.nix {
              inherit lib diskDevice;
            };
        in {
          genericlaptop = genericWorkstationDisk;
          genericdesktop = genericWorkstationDisk;
        };

      homeConfigurations = {
        "${user}" = home-manager.lib.homeManagerConfiguration {
          pkgs = pkgsFor system;
          extraSpecialArgs = {
            inherit inputs system user darkTheme lightTheme localNix;
            userModuleMode = "home";
          };
          modules = [
            ./modules/user.nix
            {
              home.username = user;
              home.homeDirectory = homeDir;
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

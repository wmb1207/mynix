# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

{
  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
      ../../modules/base.nix
    ];

  # Bootloader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  networking.hostName = "nixos"; # Define your hostname.
  networking.extraHosts =
  ''
    100.69.24.180    admin_mensajeria_test.sinacofi.cl
    100.69.24.180    mensajeria_test.sinacofi.cl
    #RB OCI
    100.66.15.24	C1PROW19PRB01
    #DEV
    100.66.51.207	C1DVUA1JB01.adb.sa-santiago-1.oraclecloud.com
    127.0.0.1 local.be.warcgroup.com
  '';
  # networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.

  # Configure network proxy if necessary
  # networking.proxy.default = "http://user:password@proxy:port/";
  # networking.proxy.noProxy = "127.0.0.1,localhost,internal.domain";

  # Enable networking
  networking.networkmanager.enable = true;

  # Set your time zone.
  time.timeZone = "America/Argentina/Buenos_Aires";

  # Select internationalisation properties.
  i18n.defaultLocale = "en_US.UTF-8";

  i18n.extraLocaleSettings = {
    LC_ADDRESS = "es_AR.UTF-8";
    LC_IDENTIFICATION = "es_AR.UTF-8";
    LC_MEASUREMENT = "es_AR.UTF-8";
    LC_MONETARY = "es_AR.UTF-8";
    LC_NAME = "es_AR.UTF-8";
    LC_NUMERIC = "es_AR.UTF-8";
    LC_PAPER = "es_AR.UTF-8";
    LC_TELEPHONE = "es_AR.UTF-8";
    LC_TIME = "es_AR.UTF-8";
  };

  # Enable the X11 windowing system.
  services.xserver.enable = true;

  # # Configure keymap in X11
  # services.xserver.xkb = {
  #   layout = "us";
  #   variant = "dvorak";
  # };

  services.xserver.libinput.enable = true;

  services.xserver.libinput.touchpad = {
    tapping = false;
  };

  # Configure console keymap
  console.keyMap = "dvorak";

  # Enable CUPS to print documents.
  services.printing.enable = true;

  # Enable sound with pipewire.
  hardware.pulseaudio.enable = false;
  hardware.bluetooth.enable = true; # enables support for Bluetooth
  hardware.bluetooth.powerOnBoot = true; # powers up the default Bluetooth controller on boot
  security.rtkit.enable = true;
  services.pipewire = {
    enable = true;
    alsa.enable = true;
    alsa.support32Bit = true;
    pulse.enable = true;
    # If you want to use JACK applications, uncomment this
    #jack.enable = true;

    # use the example session manager (no others are packaged yet so this is enabled by default,
    # no need to redefine it in your config for now)
    #media-session.enable = true;
  };
  hardware.ipu6 = {
    enable = true;
    platform = "ipu6ep";
  };

  # Enable touchpad support (enabled default in most desktopManager).
  # services.xserver.libinput.enable = true;

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.users.wmb = {
    isNormalUser = true;
    description = "wmb";
    extraGroups = [ "networkmanager" "wheel" "docker"];
     packages = with pkgs; [
       tree
       git
       emacs
       asusctl
       supergfxctl
       glxinfo
       mangohud
       protonup-qt
     ];
  };
  services.dbus.enable = true;
  programs.dconf.enable = true;

  # Install firefox.
  programs.firefox.enable = true;

  # Allow unfree packages

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages = with pkgs; [
  #  vim # Do not forget to add an editor to edit configuration.nix! The Nano editor is also installed by default.
    #  wget
    ipu6-camera-bins
    cpupower-gui
    powertop
  ];

  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  # programs.mtr.enable = true;
  # programs.gnupg.agent = {
  #   enable = true;
  #   enableSSHSupport = true;
  # };

  # List services that you want to enable:

  # Enable the OpenSSH daemon.
  # services.openssh.enable = true;

  # Open ports in the firewall.
  # networking.firewall.allowedTCPPorts = [ ... ];
  # networking.firewall.allowedUDPPorts = [ ... ];
  # Or disable the firewall altogether.
  # networking.firewall.enable = false;
  nix.settings.experimental-features = ["nix-command" "flakes"];
  services.openssh.enable = true;
  virtualisation.docker.enable = true;



  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "24.11"; # Did you read the comment?

  # Intel GPU support
  hardware.opengl = {
    enable = true;
    extraPackages = with pkgs; [
      intel-media-driver       # New VAAPI driver for Intel (replaces old iHD/i965)
      vaapiVdpau               # Needed for apps using VDPAU
      libvdpau-va-gl
      vulkan-loader
      vulkan-tools
      vulkan-validation-layers
    ];
  };

  # Firmware updates (for GPU + CPU)
  hardware.cpu.intel.updateMicrocode = true;
  hardware.enableRedistributableFirmware = true;
  hardware.enableAllFirmware = true;



  # Power management
  powerManagement.enable = true;
  
  # TLP for laptop power tuning
  services.tlp = {
    enable = true;
    settings = {
      # CPU Performance settings
      CPU_SCALING_GOVERNOR_ON_AC = "performance";
      CPU_SCALING_GOVERNOR_ON_BAT = "powersave";
    
      # Remove specific frequency limits - let the CPU decide
      # CPU_SCALING_MIN_FREQ_ON_AC = "2.0GHz";
      # CPU_SCALING_MAX_FREQ_ON_AC = "4.8GHz";
    
      # CPU Energy Performance Preference (Intel)
      CPU_ENERGY_PERF_POLICY_ON_AC = "performance";
      CPU_ENERGY_PERF_POLICY_ON_BAT = "power";
      
      # CPU Boost settings (Intel)
      CPU_BOOST_ON_AC = 1;
      CPU_BOOST_ON_BAT = 0;
      
      # CPU HWP (Hardware P-States) settings for Intel
      CPU_HWP_DYN_BOOST_ON_AC = 1;
      CPU_HWP_DYN_BOOST_ON_BAT = 0;
      
      # Platform Profile (if supported)
      PLATFORM_PROFILE_ON_AC = "performance";
      PLATFORM_PROFILE_ON_BAT = "low-power";
      
      # Battery charge thresholds
      START_CHARGE_THRESH_BAT0 = 40;
      STOP_CHARGE_THRESH_BAT0 = 80;
      
      # Runtime PM settings
      RUNTIME_PM_ON_AC = "auto";
      RUNTIME_PM_ON_BAT = "auto";
      
      # USB settings
      USB_AUTOSUSPEND = 0;  # Disable USB autosuspend on AC
      
      # WiFi power saving
      WIFI_PWR_ON_AC = "off";
      WIFI_PWR_ON_BAT = "on";
      
      # Sound card power saving
      SOUND_POWER_SAVE_ON_AC = 0;
      SOUND_POWER_SAVE_ON_BAT = 1;
    };
  };

  # Disable conflicting services
  services.power-profiles-daemon.enable = false;
  services.auto-cpufreq.enable = false;  # Make sure this is disabled too

  # Disable powertop auto-tuning (can conflict with TLP)
  powerManagement.powertop.enable = false;

  # CPU frequency scaling
  powerManagement.cpuFreqGovernor = "performance";  # This might conflict with TLP

  # Optional: Better suspend/hibernate behavior
  services.logind = {
    lidSwitch = "suspend";
    lidSwitchDocked = "ignore";
    extraConfig = ''
    HandlePowerKey=suspend
  '';
  };

  # Environment variables
  environment.sessionVariables = {
    WLR_NO_HARDWARE_CURSORS = "1";
  };

# Kernel parameters for better performance
  boot.kernelParams = [
    "intel_pstate=active"  # Use Intel P-State driver
    # "processor.max_cstate=1"  # Uncomment if you want to limit C-states
  ];

  # Optional: Enable thermald for Intel CPUs
  services.thermald.enable = true;

}

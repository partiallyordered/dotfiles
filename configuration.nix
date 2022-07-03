# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

{
  # TODO: Once Authy is updated to version, remove the permittedInsecurePackages:
  #         Source: https://github.com/NixOS/nixpkgs/blob/nixos-unstable/pkgs/applications/misc/authy/default.nix#L12
  #       Unfortunately, looks like we might be waiting a while:
  #         https://github.com/NixOS/nixpkgs/blob/nixos-unstable/pkgs/applications/misc/authy/default.nix#L12
  nixpkgs.config.permittedInsecurePackages = [ "electron-9.4.4" ];
  nixpkgs.config.allowUnfree = true;
  nixpkgs.config.android_sdk.accept_license = true;
  nix = {
    allowedUsers = [ "@wheel" ];
    package = pkgs.nixUnstable;
    extraOptions = ''
      experimental-features = nix-command flakes
    '';
  };

  # Enable all sysrq functions
  boot.kernel.sysctl."kernel.sysrq" = 1;

  boot.blacklistedKernelModules = [
    # At the time of writing, the following page states that the nouveau driver causes crashes and
    # should be blacklisted
    # https://wiki.archlinux.org/index.php/Dell_XPS_15_9570#Graphics
    # https://web.archive.org/web/20190102090447/https://wiki.archlinux.org/index.php/Dell_XPS_15_9570
    "nouveau" "rivafb" "nvidiafb" "rivatv" "nv"
    # Blacklisting this module lets us use rtl-sdr devices for purposes other than dvb
    "dvb_usb_rtl28xxu"
  ];
  boot.kernelPackages = pkgs.linuxPackages_latest;
  boot.kernelParams = [
    # Setting acpi_osi=Linux allows the BIOS to enable features supported by Linux
    "acpi_osi=Linux"
    # Setting mem_sleep_default=deep selects the more efficient 'deep' sleep mode as default
    "mem_sleep_default=deep"
    # Turn off speculative execution vulnerability mitigations. Trusting the browser sandbox to
    # save us now.
    "mitigations=off"
  ];
  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;
  # TODO: currently this is not working:
  # Turn off the GPU at boot
  boot.kernelModules = [ "acpi_call" "wireguard" ];
  boot.extraModulePackages = with config.boot.kernelPackages; [ acpi_call ];
  # https://wiki.archlinux.org/index.php/Dell_XPS_15_9560#Enable_power_saving_features_for_the_i915_kernel_module
  boot.extraModprobeConfig = "options i915 enable_fbc=1 enable_psr=1 disable_power_well=0";
  # TODO: do we need to run this when we exit suspend/hibernate? Does the discrete GPU turn on
  # again?
  # TODO: this doesn't appear to be supported any longer
  # We should still be able to run
  #   echo "\_SB.PCI0.PEG0.PEGP._OFF" > /proc/acpi/call
  # boot.systemd.tmpfiles.rules = [ "w /proc/acpi/call - - - - \\_SB.PCI0.PEG0.PEGP._OFF" ];
  boot.supportedFilesystems = [ "f2fs" ];

  virtualisation.docker = {
    enable = true;
    autoPrune.enable = true; # default schedule weekly
  };

  security.sudo = {
    enable = true;
    wheelNeedsPassword = false;
  };

  networking.useNetworkd = true;
  networking.useDHCP = false; # Not compatible with networkd
  # IWD settings:
  # https://search.nixos.org/options?channel=unstable&show=networking.wireless.iwd.settings&from=0&size=50&sort=relevance&type=packages&query=iwd
  networking.connman = {
    enable = true;
    wifi.backend = "iwd";
    enableVPN = false;
  };
  # https://search.nixos.org/options?channel=unstable&from=0&size=50&sort=relevance&type=packages&query=networking.wireless
  networking.wireless.iwd = {
    enable = true;
  };

  networking.wireguard.enable = true;
  networking.firewall.checkReversePath = "loose";
  services.mullvad-vpn.enable = true;
  services.upower = {
    enable = true;
    criticalPowerAction = "Hibernate";
    # The battery is in bad condition, at the time of writing
    percentageAction = 10;
    percentageCritical = 15;
    percentageLow = 30;
  };

  # https://nixos.wiki/wiki/Fonts
  fonts.fonts = with pkgs; [
    dejavu_fonts
    fira-code
    fira-code-symbols
    font-awesome
    inconsolata
    liberation_ttf
    material-design-icons
    (nerdfonts.override { fonts = [ "FiraCode" "DroidSansMono" ]; })
    noto-fonts
    noto-fonts-cjk
    noto-fonts-emoji
    powerline-fonts
    terminus_font
    ttf_bitstream_vera
    (callPackage ./fonts/dejavuwififont.nix {})
    (callPackage ./fonts/steadysetsfont.nix {})
    # vistafonts # marked as broken
  ];

  systemd.network = {
    enable = true;
    # See man systemd.netdev
    # See https://wiki.archlinux.org/title/Mullvad#With_systemd-networkd
    # https://nixos.wiki/wiki/WireGuard#Setting_up_WireGuard_with_NetworkManager
    # See
    # - man systemd.network
    # - https://wiki.archlinux.org/title/systemd-networkd
    networks = let
      networkConfig = {
        DHCP = "yes";
        DNSSEC = "yes";
        DNSOverTLS = "yes";
        DNS = [ "1.0.0.1" "1.1.1.1" ];
      };
    in {
      "40-wired" = {
        enable = true;
        name = "en*";
        inherit networkConfig;
        dhcpV4Config.RouteMetric = 1024;
      };
      "40-wireless" = {
        enable = true;
        name = "wl*";
        inherit networkConfig;
        dhcpV4Config.RouteMetric = 2048;
      };
    };
  };

  # Select internationalisation properties.
  i18n = {
    defaultLocale = "en_GB.UTF-8";
  };
  console = {
    font = "Lat2-Terminus16";
    keyMap = "uk";
  };

  # Set your time zone.
  time.timeZone = "Europe/London";

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages = with pkgs; [
    # TODO: binutils-unwrapped was added as a stop-gap measure to prevent a dependency clash. It
    # may no longer be necessary here.
    binutils-unwrapped
    # for usage in root
    curl neovim
    # for drivers
    exfat rtl-sdr
  ];

  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  # programs.mtr.enable = true;
  programs.zsh.enable = true;

  # List services that you want to enable:

  # Enable the OpenSSH daemon.
  # services.openssh.enable = true;

  # Open ports in the firewall.
  # networking.firewall.allowedTCPPorts = [ ... ];
  # networking.firewall.allowedUDPPorts = [ ... ];
  # Or disable the firewall altogether.
  # networking.firewall.enable = false;
  services.logind = {
    lidSwitch = "suspend";
    extraConfig = ''
      HandlePowerKey=suspend
    '';
  };

  # Android development
  programs.adb.enable = true;

  # System-wide non-x-dependent backlight control
  programs.light.enable = true;
  services.actkbd = {
    enable = true;
    bindings = [
      # backlight control
      { keys = [ 224 ]; events = [ "key" ]; command = "${pkgs.light}/bin/light -U 5"; }
      { keys = [ 225 ]; events = [ "key" ]; command = "${pkgs.light}/bin/light -A 5"; }
    ];
  };

  # graphics
  # hardware.nvidiaOptimus.disable = true;
  # hardware.opengl.extraPackages = [ pkgs.linuxPackages.nvidia_x11.out ];
  # hardware.opengl.extraPackages32 = [ pkgs_i686.linuxPackages.nvidia_x11.out ];

  services.transmission.enable = true;

  # Enable sound.
  sound.enable = true;
  hardware.pulseaudio = {
    enable = true;
    zeroconf.discovery.enable = true;
    package = pkgs.pulseaudioFull;
  };

  # TODO: extraConfig possibly not necessary
  hardware.bluetooth = {
    settings = {
      General = {
        Enable = "Source,Sink,Media,Socket";
      };
    };
    enable = true;
  };

  hardware.keyboard.zsa.enable = true;

  hardware.enableRedistributableFirmware = true;

  # Enable the X11 windowing system.
  services.xserver = {
    # need a display manager, apparently
    displayManager.lightdm.enable = true;
    displayManager.autoLogin.enable = true;
    displayManager.autoLogin.user = "msk";
    enable = true;
    layout = "gb";
    xkbOptions = "eurosign:e";
    autorun = true;
    # Enable touchpad support. Disable touchpad while typing.
    libinput.enable = true;
    libinput.touchpad.disableWhileTyping = true;
    # Actual display size from Dell docs at: https://web.archive.org/web/20181127175408/https://www.dell.com/support/manuals/uk/en/ukbsdt1/xps-15-9570-laptop/xps-15-9570-setupandspecifications/display?guid=guid-c01eaef3-9cab-4786-83d5-c02385013cb7&lang=en-us
    # 344mm x 194mm
    # TODO:
    # even when this section is specified, `xrandr --props` returns (note dimensions at the end):
    # eDP1 connected primary 3840x2160+0+0 (normal left inverted right x axis y axis) 340mm x 190mm
    xrandrHeads = [
      {
        output = "eDP-1";
        primary = true;
        monitorConfig = ''
          DisplaySize 340 190
        '';
      }
      {
        output = "DP-1-2";
        primary = false;
        monitorConfig = ''
          DisplaySize 509 286
        '';
      }
      {
        output = "DP-2-1";
        primary = false;
        monitorConfig = ''
          DisplaySize 509 286
        '';
      }
    ];
  };

  services.k3s.enable = false;

  # Enable the KDE Desktop Environment.
  # services.xserver.displayManager.sddm.enable = true;
  # services.xserver.desktopManager.plasma5.enable = true;

  users.defaultUserShell = pkgs.zsh;
  # Define a user account.
  # Generate a hashed pw with `nix-shell -p mkpasswd --command 'mkpasswd'`
  users.users.msk = {
    isNormalUser = true;
    home = "/home/msk";
    extraGroups = [
      "wheel"
      "docker"
      "wireshark"
      "dialout"
      "adbusers"
      "plugdev"
      "video" # For backlight control, see https://nixos.wiki/wiki/Backlight#light
    ];
    uid = 1000;
    hashedPassword = "$6$xfdEQ0tZTs34sENv$fmyZ/F4U/K8OSuTk5z61lurH0xNnNPpfh.mQAh0zOl8qawmiz2EZ5zbZx/esIhyJyC0lPv1EFAF66BvLUr3es0";
  };
  users.users.test = {
    isNormalUser = true;
    home = "/home/test";
    extraGroups = [ "wheel" ];
    uid = 1001;
  };

  # services.openssh = {
  #   enable = true;
  # };

  users.groups.wireshark = {};

  # This value determines the NixOS release with which your system is to be
  # compatible, in order to avoid breaking some software such as database
  # servers. You should change this only after NixOS release notes say you
  # should.
  system.stateVersion = "18.09"; # Did you read the comment?

  system.activationScripts = {
    rfkillUnblockWlan = {
      text = ''
      rfkill unblock wlan
      '';
      deps = [];
    };
  };
}

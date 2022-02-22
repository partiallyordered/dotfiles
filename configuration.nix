# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

{
  nixpkgs.config.allowUnfree = true;
  nixpkgs.config.android_sdk.accept_license = true;
  nix = {
    allowedUsers = [ "@wheel" ];
    package = pkgs.nixUnstable;
    extraOptions = ''
      experimental-features = nix-command flakes
    '';
  };

  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
    ];

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

  networking.hostName = "nixos"; # Define your hostname.
  networking.useNetworkd = true;
  networking.firewall.checkReversePath = false; # https://nixos.wiki/wiki/WireGuard#Setting_up_WireGuard_with_NetworkManager
  networking.useDHCP = false; # Not compatible with networkd
  networking.wireless = {
    enable = true;
    userControlled.enable = true;
    networks = {
      # To add a new network, run
      #   SSID="my_ssid"
      #   PASSWORD="my_password"
      #   wpa_passphrase "$SSID" "$PASSWORD"
      # and transpose the resulting psk here.
      # See also: https://nixos.wiki/wiki/Wpa_supplicant
      "Lucy Goosey" = {
        pskRaw = "89bcc6d8b7ac2b2d41d8f705e09661d5a62cb9c4320509f799440a68e685332a";
      };
      pxl = {
        pskRaw = "b0d5e9a2f07973f9d8038fb71a2cc1c439bab3aaa75e404be09eeaf01bfefc01";
      };
      Pixel = {
        pskRaw = "4f16b9e4567bf187eeb030f660ba77d805d5d6e74cc7eae19529afb78a5d5abd";
      };
      TP-Link_AD20 = {
        pskRaw = "7d30414caa7ed8a24811266de30e02c9a0c077f1eb1f8609481061c28e069cb7";
      };
    };
  };
  systemd.network = {
    enable = true;
    # See man systemd.netdev
    # See https://wiki.archlinux.org/title/Mullvad#With_systemd-networkd
    # https://nixos.wiki/wiki/WireGuard#Setting_up_WireGuard_with_NetworkManager
    netdevs = {
      "10-wg0" = {
        netdevConfig = {
          Kind = "wireguard";
          MTUBytes = "1300";
          Name = "wg0";
        };
        # See also man systemd.netdev (also contains info on the permissions of the key files)
        # Get the base64 encoded private key from a wireguard config. Save it in a new file at
        # /etc/wireguard/key. From the systemd.netdev manual:
        # > Note that the file must be readable by the user "systemd-network", so it should be,
        # > e.g., owned by "root:systemd-network" with a "0640" file mode.
        # Probably
        # sudo chown -R systemd-network:root /etc/wireguard/key
        # sudo chmod 0640 /etc/wireguard/key
        extraConfig = ''
          [WireGuard]
          PrivateKeyFile=/etc/wireguard/key
          FirewallMark=0x8888
          ListenPort=51820

          [WireGuardPeer]
          PublicKey=IJJe0TQtuQOyemL4IZn6oHEsMKSPqOuLfD5HoAWEPTY=
          AllowedIPs=0.0.0.0/0
          AllowedIPs=::0/0
          Endpoint=141.98.252.130:51820
        '';
      };
    };
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
      # Maybe this interface can default to down; we can have a large list of loaded interfaces,
      # and all can be down by default, then to use a VPN endpoint, we can just bring the endpoint
      # up.
      # See https://wiki.archlinux.org/title/Mullvad#With_systemd-networkd
      # https://nixos.wiki/wiki/WireGuard#Setting_up_WireGuard_with_NetworkManager
      "40-wg0" = {
        extraConfig = ''
          [Match]
          Name=wg0

          [Route]
          Gateway=0.0.0.0
          Table=1000

          [Route]
          Gateway=::
          Table=1000

          [Network]
          DNS=193.138.218.74
          DNS=100.64.0.3
          DNSDefaultRoute=yes
          Domains=~.
          Address=10.67.52.225/32
          Address=fc00:bbbb:bbbb:bb01::4:34e0/128

          [RoutingPolicyRule]
          Family=both
          SuppressPrefixLength=0
          Priority=999
          Table=main

          [RoutingPolicyRule]
          Family=both
          FirewallMark=0x8888
          InvertRule=true
          Table=1000
          Priority=1000
        '';
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

  services.physlock.enable = true;

  # System-wide non-x-dependent backlight control
  programs.light.enable = true;
  services.actkbd = {
    enable = true;
    bindings = [
      # backlight control
      { keys = [ 224 ]; events = [ "key" ]; command = "${pkgs.light}/bin/light -U 5"; }
      { keys = [ 225 ]; events = [ "key" ]; command = "${pkgs.light}/bin/light -A 5"; }
      # media keys
      { keys = [ 113 ]; events = [ "key" ]; command = "${pkgs.alsaUtils}/bin/amixer -q set Master toggle"; }
      # { keys = [ 113 ]; events = [ "key" ]; command = "${pkgs.alsaUtils}/bin/amixer -q set Master 5%+"; }
      # { keys = [ 114 ]; events = [ "key" ]; command = "${pkgs.alsaUtils}/bin/light -U 10"; }
      # { keys = [ 114 ]; events = [ "key" ]; command = "${pkgs.alsaUtils}/bin/amixer -q set Master 5%+"; }
      # { keys = [ 115 ]; events = [ "key" ]; command = "${pkgs.alsaUtils}/bin/amixer -q set Master 5%-"; }
      # back: { keys = [ 165 ]; events = [ "key" ]; command = "${pkgs.alsaUtils}/bin/amixer -q set Master 5%+"; }
      # pause/play: { keys = [ 164 ]; events = [ "key" ]; command = "${pkgs.alsaUtils}/bin/amixer -q set Master 5%+"; }
      # forward: { keys = [ 163 ]; events = [ "key" ]; command = "${pkgs.alsaUtils}/bin/amixer -q set Master 5%+"; }
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
    extraModules = [ pkgs.pulseaudio-modules-bt ];
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

  # Enable autorandr
  services.autorandr.enable = true;
  # TODO: built-in options are "horizontal", "vertical", "common". "common" is the same as
  # "mirrored" in Windows. The others are probably what you think they are.
  services.autorandr.defaultTarget = "horizontal";

  services.k3s.enable = false;

  # Enable the KDE Desktop Environment.
  # services.xserver.displayManager.sddm.enable = true;
  # services.xserver.desktopManager.plasma5.enable = true;

  users.defaultUserShell = pkgs.zsh;
  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.users.msk = {
    isNormalUser = true;
    home = "/home/msk";
    extraGroups = [ "wheel" "networkmanager" "docker" "wireshark" "dialout" "adbusers" "plugdev" ];
    uid = 1000;
  };
  users.users.test = {
    isNormalUser = true;
    home = "/home/test";
    extraGroups = [ "wheel" ];
    uid = 1001;
  };

  users.groups.wireshark = {};

  # This value determines the NixOS release with which your system is to be
  # compatible, in order to avoid breaking some software such as database
  # servers. You should change this only after NixOS release notes say you
  # should.
  system.stateVersion = "18.09"; # Did you read the comment?

}

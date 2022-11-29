# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

{
  # TODO: Once Authy is updated to version, remove the permittedInsecurePackages:
  #         Source: https://github.com/NixOS/nixpkgs/blob/nixos-unstable/pkgs/applications/misc/authy/default.nix#L12
  #       Unfortunately, looks like we might be waiting a while:
  #         https://github.com/NixOS/nixpkgs/blob/nixos-unstable/pkgs/applications/misc/authy/default.nix#L12
  nixpkgs.config = {
    permittedInsecurePackages = [ "electron-9.4.4" ];
    allowUnfree = true;
    android_sdk.accept_license = true;
    packageOverrides = pkgs: {
      # hello = pkgs.hello.overrideAttrs (oldAttrs: {
      #   separateDebugInfo = true;
      # });
    };
  };
  nix = {
    package = pkgs.nixUnstable;
    extraOptions = ''
      experimental-features = nix-command flakes
    '';
    settings = {
      allowed-users = [ "@wheel" ];
      # As recommended here: https://nixos.wiki/wiki/Storage_optimization
      auto-optimise-store = true;
      # TODO: garbage collection automation? https://nixos.wiki/wiki/Storage_optimization#Automation
    };
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
  # The Zen kernel prioritises responsiveness over throughput
  # Some patches we might be interested in:
  # - MGLRU, in kernel 6.2
  # - le9 patches for low memory systems
  # - BFQ I/O scheduler
  # - MuQSS
  # See also:
  # - xanmod kernel
  # - https://nixos.wiki/wiki/Linux_kernel
  # - https://wiki.archlinux.org/title/kernel#Unofficial_kernels
  # - https://search.nixos.org/options?channel=22.05&show=boot.kernelPatches&from=0&size=50&sort=relevance&type=packages&query=boot.kernelPatches
  # - https://wiki.archlinux.org/title/zswap
  # - generally search for techniques to improve interactivity and responsiveness in desktop linux
  boot.kernelPackages = pkgs.linuxPackages_zen;
  boot.kernelParams = [
    # Setting acpi_osi=Linux allows the BIOS to enable features supported by Linux
    "acpi_osi=Linux"
    # Setting mem_sleep_default=deep selects the more efficient 'deep' sleep mode as default
    "mem_sleep_default=deep"
    # Turn off speculative execution vulnerability mitigations. Trusting the browser sandbox to
    # save us now.
    "mitigations=off"
    # TODO: setting zswap.compressor=zstd is not currently working. This can be seen as follows:
    #         # journalctl -b | grep zstd
    #         zswap: compressor zstd not available, using default lzo
    #       In theory, boot.initrd.kernelModules should be able to rectify this:
    #         boot.initrd.kernelModules = [ "zstd" ];
    #       but this does not work.
    #       It might also be possible to configure the kernel, see: https://nixos.wiki/wiki/Linux_kernel#Custom_configuration
    #       And see for options:
    #         # zgrep "\(ZSWAP\|ZSTD\)" /proc/config.gz
    #       One other possibility could be:
    #         # echo 1 > /sys/module/zswap/parameters/enabled
    #       See also:
    #       - https://discourse.nixos.org/t/how-to-activate-zswap-using-zstd-at-boot-time/12462/3
    #       - https://wiki.archlinux.org/title/zswap
    #       - https://wiki.gentoo.org/wiki/Zstd
    #       - https://old.reddit.com/r/linuxquestions/comments/ju4bft/zram_zswap_or_both/
    #       - https://askubuntu.com/a/472227
    "zswap.enabled=1"
    "zswap.compressor=zstd"
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
  # TODO: this seems to significantly reduce battery power consumption, by about 15W
  # TODO: add a polybar module to detect the state of this?
  # TODO: automatically set this ACPI setting when the power is unplugged?
  # TODO: see the arch linux page on XPS 15 9570 (and 9560 and others as relevant)
  # TODO: do the open source nvidia drivers have any effect on power management?
  #       - https://wiki.archlinux.org/title/NVIDIA_Optimus
  # We should still be able to run
  #   echo "\_SB.PCI0.PEG0.PEGP._OFF" > /proc/acpi/call
  # boot.systemd.tmpfiles.rules = [ "w /proc/acpi/call - - - - \\_SB.PCI0.PEG0.PEGP._OFF" ];
  boot.supportedFilesystems = [ "f2fs" ];

  virtualisation.docker = {
    enable = true;
    autoPrune.enable = true;
    # TODO: rootless.enable = true;
  };

  virtualisation.podman = {
    enable = true;
    # One day this should be updated to use the netavark network backend, which supports hostname
    # resolution by default, instead of the dnsname plugin
    defaultNetwork.dnsname.enable = true;
  };

  security.sudo = {
    enable = true;
    wheelNeedsPassword = true;
  };

  security.pam.loginLimits = [
    { type = "-"; item = "nice"; value = "-20"; domain = "msk"; }
  ];

  services.usbmuxd.enable = true; # per https://nixos.wiki/wiki/IOS

  services.clamav = {
    daemon.enable = true;
    updater.enable = true;
  };

  networking.useNetworkd = true;
  networking.useDHCP = false; # Not compatible with networkd
  # IWD settings:
  # https://search.nixos.org/options?channel=unstable&show=networking.wireless.iwd.settings&from=0&size=50&sort=relevance&type=packages&query=iwd
  services.connman = {
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
    percentageAction = 5;
    percentageCritical = 10;
    percentageLow = 15;
  };

  # See also:
  # - https://github.com/hakavlad/nohang#solution
  # - systemd-oomd
  #   - note that systemd-oomd works with cgroups instead of processes, therefore will not kill
  #     only e.g. a single browser tab, but rather the whole browser.
  #   - built with https://github.com/facebookincubator/oomd
  services.earlyoom = {
    enable = true;
    enableNotifications = true;
    extraArgs = [ "-g" "--prefer '(^|/)(picom|java|chromium)$'" ];
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
    wait-online.anyInterface = true;
    wait-online.timeout = 5;
    # See man systemd.netdev
    # See https://wiki.archlinux.org/title/Mullvad#With_systemd-networkd
    # https://nixos.wiki/wiki/WireGuard#Setting_up_WireGuard_with_NetworkManager
    # See
    # - man systemd.network
    # - https://wiki.archlinux.org/title/systemd-networkd
    networks = let
      # TODO: what's a more simple form of these settings? Are these the reason I have no
      # connectivity when I'm on VPN?
      networkConfig = {
        DHCP = "yes";
        DNSSEC = "yes";
        DNSOverTLS = "yes";
        DNS = [ "1.0.0.1" "1.1.1.1" ];
      };
    in {
      "40-wired" = {
        enable = true;
        name = "en* eth*";
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
    # ifuse and libimobiledevice for iphone USB tethering and file system mount
    libimobiledevice
    ifuse
  ];

  # We enable this here instead of in the user configuration because it requires firewall ports
  # open
  programs.kdeconnect.enable = true;

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

  # From: https://nixos.wiki/wiki/Steam
  programs.steam = {
    enable = true;
    remotePlay.openFirewall = true; # Open ports in the firewall for Steam Remote Play
    dedicatedServer.openFirewall = true; # Open ports in the firewall for Source Dedicated Server
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
    # TODO: attempt to increase max clients because sometimes I run out of terminal windows!
    # extraConfig = ''
    #   # https://man.archlinux.org/man/extra/xorg-server/xorg.conf.d.5.en
    #   # Determine allowable max clients as follows:
    #   #   $ Xorg -maxclients 1000000000
    #   #   (EE)
    #   #   Fatal server error:
    #   #   (EE) maxclients must be one of 64, 128, 256, 512, 1024 or 2048
    #   #   (EE)
    #   #   (EE)
    #   #   Please consult the The X.Org Foundation support
    #   #        at http://wiki.x.org
    #   #    for help.
    #   #   (EE)
    #   Section "ServerFlags"
    #           Option MaxClients "2048"
    #   EndSection
    # '';
    # need a display manager, apparently
    # See lightdm and DPI in this section: https://linuxreviews.org/HOWTO_set_DPI_in_Xorg#How_To_Permanently_Set_Your_Display_DPI
    displayManager.lightdm.enable = true;
    displayManager.autoLogin.enable = true;
    displayManager.autoLogin.user = "msk";
    displayManager.session = [
      {
        manage = "desktop";
        name = "myxmonad";
        start = ''exec $HOME/.xsession'';
      }
    ];
    displayManager.defaultSession = "myxmonad";
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

  users.defaultUserShell = pkgs.zsh;
  # TODO: replace all references to "msk" with a variable
  # Define a user account.
  users.users.msk = {
    isNormalUser = true;
    home = "/home/msk";
    extraGroups = [
      "wheel"
      "docker"
      "podman"
      "wireshark"
      "dialout"
      "adbusers"
      "plugdev"
      "video" # For backlight control, see https://nixos.wiki/wiki/Backlight#light
    ];
    uid = 1000;
    # Generate a hashed pw with `nix-shell -p mkpasswd --command 'mkpasswd'`
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
  system.stateVersion = "22.05"; # Did you read the comment?

  system.activationScripts = {
    rfkillUnblockWlan = {
      text = ''
      rfkill unblock wlan
      '';
      deps = [];
    };
  };
}

# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

{
  nixpkgs.config.allowUnfree = true;
  nix.allowedUsers = [ "@wheel" ];

  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
    ];

  # At the time of writing, the following page states that the nouveau driver causes crashes and
  # should be blacklisted
  # https://wiki.archlinux.org/index.php/Dell_XPS_15_9570#Graphics
  # https://web.archive.org/web/20190102090447/https://wiki.archlinux.org/index.php/Dell_XPS_15_9570
  boot.blacklistedKernelModules = [ "nouveau" "rivafb" "nvidiafb" "rivatv" "nv" "uvcvideo" ];
  boot.kernelPackages = pkgs.linuxPackages_latest;
  boot.kernelParams = [ "acpi_osi=Linux" ];
  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  virtualisation.docker.enable = true;

  security.sudo = {
    enable = true;
    wheelNeedsPassword = false;
  };

  networking.hostName = "nixos"; # Define your hostname.
  networking.networkmanager.enable = true;
  networking.nameservers = [ "1.1.1.1" "1.0.0.1" "9.9.9.9" ];

  # Configure network proxy if necessary
  # networking.proxy.default = "http://user:password@proxy:port/";
  # networking.proxy.noProxy = "127.0.0.1,localhost,internal.domain";

  # Select internationalisation properties.
  i18n = {
    consoleFont = "Lat2-Terminus16";
    consoleKeyMap = "uk";
    defaultLocale = "en_GB.UTF-8";
  };

  # Set your time zone.
  time.timeZone = "Europe/London";

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages = with pkgs; [
    curl neovim
  ];

  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  # programs.mtr.enable = true;
  # programs.gnupg.agent = { enable = true; enableSSHSupport = true; };
  programs.zsh.enable = true;

  # List services that you want to enable:

  # Enable the OpenSSH daemon.
  # services.openssh.enable = true;

  # Open ports in the firewall.
  # networking.firewall.allowedTCPPorts = [ ... ];
  # networking.firewall.allowedUDPPorts = [ ... ];
  # Or disable the firewall altogether.
  # networking.firewall.enable = false;

  # Enable CUPS to print documents.
  # services.printing.enable = true;

  services.logind.lidSwitch = "hibernate";
  programs.slock.enable = true;

  # System-wide non-x-dependent backlight control
  programs.light.enable = true;
  services.actkbd = {
    enable = true;
    bindings = [
      # backlight control
      { keys = [ 224 ]; events = [ "key" ]; command = "${pkgs.light}/bin/light -U 10"; }
      { keys = [ 225 ]; events = [ "key" ]; command = "${pkgs.light}/bin/light -A 10"; }
      # media keys
      # mute: { keys = [ 113 ]; events = [ "key" ]; command = "${pkgs.alsaUtils}/bin/amixer -q set Master 5%+"; }
      { keys = [ 114 ]; events = [ "key" ]; command = "${pkgs.alsaUtils}/bin/light -U 10"; }
      # { keys = [ 114 ]; events = [ "key" ]; command = "${pkgs.alsaUtils}/bin/amixer -q set Master 5%+"; }
      { keys = [ 115 ]; events = [ "key" ]; command = "${pkgs.alsaUtils}/bin/amixer -q set Master 5%-"; }
      # back: { keys = [ 165 ]; events = [ "key" ]; command = "${pkgs.alsaUtils}/bin/amixer -q set Master 5%+"; }
      # pause/play: { keys = [ 164 ]; events = [ "key" ]; command = "${pkgs.alsaUtils}/bin/amixer -q set Master 5%+"; }
      # forward: { keys = [ 163 ]; events = [ "key" ]; command = "${pkgs.alsaUtils}/bin/amixer -q set Master 5%+"; }
    ];
  };

  # graphics
  # hardware.nvidiaOptimus.disable = true;
  # hardware.opengl.extraPackages = [ pkgs.linuxPackages.nvidia_x11.out ];
  # hardware.opengl.extraPackages32 = [ pkgs_i686.linuxPackages.nvidia_x11.out ];

  # Enable sound.
  sound.enable = true;
  hardware.pulseaudio = {
    enable = true;
    zeroconf.discovery.enable = true;
    package = pkgs.pulseaudioFull;
    extraModules = [ pkgs.pulseaudio-modules-bt ];
  };

  hardware.bluetooth.enable = true;

  # Enable the X11 windowing system.
  services.xserver = {
    # displayManager.lightdm.enable = true;
    enable = true;
    # videoDrivers = ["nvidia" "intel"];
    layout = "gb";
    xkbOptions = "eurosign:e";
    # windowManager.xmonad = {
    #   enable = true;
    #   enableContribAndExtras = true;
    #   extraPackages = haskellPackages: [
    #     haskellPackages.xmonad-contrib
    #     haskellPackages.xmonad-extras
    #     haskellPackages.xmonad
    #   ];
    # };
    autorun = true;
    # windowManager.default = "xmonad";
    libinput.enable = true;
    # Actual display size from Dell docs at: https://web.archive.org/web/20181127175408/https://www.dell.com/support/manuals/uk/en/ukbsdt1/xps-15-9570-laptop/xps-15-9570-setupandspecifications/display?guid=guid-c01eaef3-9cab-4786-83d5-c02385013cb7&lang=en-us
    monitorSection = ''
      DisplaySize 344 194
    '';
  };
  # services.xserver.enable = true;
  # services.xserver.layout = "us";
  # services.xserver.xkbOptions = "eurosign:e";

  # Enable touchpad support.
  # services.xserver.libinput.enable = true;

  # Enable the KDE Desktop Environment.
  # services.xserver.displayManager.sddm.enable = true;
  # services.xserver.desktopManager.plasma5.enable = true;

  users.defaultUserShell = pkgs.zsh;
  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.users.msk = {
    isNormalUser = true;
    home = "/home/msk";
    extraGroups = [ "wheel" "networkManager" "docker" ];
    uid = 1000;
  };

  # This value determines the NixOS release with which your system is to be
  # compatible, in order to avoid breaking some software such as database
  # servers. You should change this only after NixOS release notes say you
  # should.
  system.stateVersion = "18.09"; # Did you read the comment?

}

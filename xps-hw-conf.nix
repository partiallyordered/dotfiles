# Do not modify this file!  It was generated by ‘nixos-generate-config’
# and may be overwritten by future invocations.  Please make changes
# to /etc/nixos/configuration.nix instead.
{ config, lib, pkgs, ... }:

# TODO:
# Inspect: https://github.com/NixOS/nixos-hardware/tree/master/dell/xps

# Encrypted disk install
# https://gist.github.com/domenkozar/b3c945035af53fa816e0ac460f1df853
# https://web.archive.org/web/20181204111837/https://gist.github.com/domenkozar/b3c945035af53fa816e0ac460f1df853

{
  # TODO: hostname is not hw-conf; we should either rename this file, or move non-hw stuff out of
  # it.
  networking.hostName = "xps"; # Define your hostname.

  boot.initrd.availableKernelModules = [ "xhci_pci" "ahci" "nvme" "usb_storage" "sd_mod" "rtsx_pci_sdmmc" ];
  boot.initrd.checkJournalingFS = false;
  boot.kernelModules = [ "kvm-intel" ];
  boot.extraModulePackages = [ ];
  boot.supportedFilesystems = [ "f2fs" ];
  # TODO: is this still necessary? Try disable it.
  boot.zfs.enableUnstable = true;

  # # lsblk -o +UUID
  # NAME          MAJ:MIN RM   SIZE RO TYPE  MOUNTPOINT     UUID
  # loop0           7:0    0     1G  1 loop  /nix/.ro-store 
  # sda             8:0    1  57.9G  0 disk                 1970-01-01-00-00-01-00
  # ├─sda1          8:1    1   1.1G  0 part  /iso           1970-01-01-00-00-01-00
  # └─sda2          8:2    1    20M  0 part                 1234-5678
  # nvme0n1       259:0    0 953.9G  0 disk                 
  # ├─nvme0n1p1   259:4    0     3M  0 part                 120a0448-d010-4b22-be19-333a3b4ad847
  # │ └─cryptkey  254:0    0     1M  0 crypt                
  # ├─nvme0n1p2   259:5    0    32G  0 part                 9b4a78dd-0cf1-4a9e-b16e-3ddddf171bb4
  # │ └─cryptswap 254:1    0    32G  0 crypt [SWAP]         e8b1a2d5-c173-4de1-af4e-3f1f1d814e82
  # ├─nvme0n1p3   259:6    0 921.4G  0 part                 4006f712-b059-4800-ad98-4034b76b787f
  # │ └─cryptroot 254:2    0 921.4G  0 crypt /mnt           bac3ac75-f1b4-43e1-9cb3-3d8816dc2764
  # └─nvme0n1p4   259:7    0   512M  0 part  /mnt/boot      AE40-3E6C

  fileSystems."/" =
    { device = "/dev/disk/by-uuid/bac3ac75-f1b4-43e1-9cb3-3d8816dc2764";
      fsType = "f2fs";
    };

  # boot.initrd.luks.devices."cryptroot".device = "/dev/disk/by-uuid/4006f712-b059-4800-ad98-4034b76b787f";
  boot.initrd.luks.devices = {
    cryptkey = {
      device = "/dev/disk/by-uuid/120a0448-d010-4b22-be19-333a3b4ad847";
    };

    cryptroot = {
      device = "/dev/disk/by-uuid/4006f712-b059-4800-ad98-4034b76b787f";
      keyFile = "/dev/mapper/cryptkey";
    };

    cryptswap = {
      device = "/dev/disk/by-uuid/9b4a78dd-0cf1-4a9e-b16e-3ddddf171bb4";
      keyFile = "/dev/mapper/cryptkey";
    };
  };

  fileSystems."/boot" =
    { device = "/dev/disk/by-uuid/AE40-3E6C";
      fsType = "vfat";
    };

  swapDevices =
    [ { device = "/dev/disk/by-uuid/e8b1a2d5-c173-4de1-af4e-3f1f1d814e82"; }
    ];

  nix.maxJobs = lib.mkDefault 12;
  powerManagement.cpuFreqGovernor = lib.mkDefault "powersave";
}

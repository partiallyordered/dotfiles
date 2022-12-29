
### Boot fails
1. Boot into live usb/cd
2. Mount file systems
    2.1. Decrypt file systems if necessary
    ```sh
    lsblk
    # the key will probably be a couple of mb
    sudo cryptsetup luksOpen /dev/nvme0n1p1 cryptkey
    sudo cryptsetup luksOpen --key-file=/dev/mapper/cryptkey /dev/nvme0n1p2 cryptswap
    sudo cryptsetup luksOpen --key-file=/dev/mapper/cryptkey /dev/nvme0n1p3 cryptroot
    ```
    2.2. Mount
    ```sh
    sudo mount /dev/mapper/cryptroot /mnt
    sudo mount /dev/nvme0n1p4 /mnt/boot
    ```
    2.3. Turn on swap
    ```sh
    swapon /dev/mapper/cryptswap
    ```
3. Reinstall, or chroot
```sh
# chroot:
sudo nixos-enter
# reinstall:
sudo nixos-install
```

### See Kernel options
Currently running kernel:
```sh
nvim /proc/config.gz
```
Some kernel package:
```sh
# $HOST is expected to be the system hostname
bat $(nix build --print-out-paths --no-link ".#nixosConfigurations.$HOST.config.boot.kernelPackages.kernel.configfile")
```

Some kernel package, in this case `pkgs.linuxPackages_latest`, but could be e.g.
`pkgs.linuxPackages_zen`:
```sh
# nixpkgs
bat $(nix build --print-out-paths --no-link 'nixpkgs#linuxPackages_latest.kernel.configfile')
# some other package source
bat $(nix build --print-out-paths --no-link 'sys#linuxPackages_latest.kernel.configfile')
```

{
  description = "Alan Stern's usbreset.c";
  # - https://marc.info/?l=linux-usb&m=121459435621262&w=2
  # - https://askubuntu.com/questions/645/how-do-you-reset-a-usb-device-from-the-command-line

  inputs = {
    usbResetSrc = {
      url = "github:Strykar/usbreset";
      flake = false;
    };
    nixpkgs.url = github:nixos/nixpkgs/nixos-unstable;
  };

  outputs = { self, nixpkgs, usbResetSrc }: {
    packages.x86_64-linux.usbreset = with import nixpkgs { system = "x86_64-linux"; };
    stdenv.mkDerivation {
      name = "";
      src = usbResetSrc;
      buildPhase = ''
        cd $src
        mkdir -p $out/bin
        cc usbreset.c -o $out/bin/usbreset
        chmod +x $out/bin/usbreset
      '';
    };

    packages.x86_64-linux.default = self.packages.x86_64-linux.usbreset;
  };
}

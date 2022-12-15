{
  description = "A very basic flake";

  inputs = {
    gorisBin = {
      url = "https://github.com/tanaikech/goris/releases/download/v3.0.1/goris_linux_amd64";
      flake = false;
    };
    nixpkgs.url = github:nixos/nixpkgs/nixos-unstable;
  };

  outputs = { self, nixpkgs, gorisBin }: {
    # packages.x86_64-linux = with import nixpkgs { system = "x86_64-linux"; };

    packages.x86_64-linux.goris = with import nixpkgs { system = "x86_64-linux"; };
    stdenv.mkDerivation {
      name = "goris";
      src = gorisBin;
      dontUnpack = true;
      installPhase = ''
        mkdir -p $out/bin
        cp $src $out/bin/goris
        chmod +x $out/bin/goris
      '';
    };

    packages.x86_64-linux.default = self.packages.x86_64-linux.goris;
  };
}

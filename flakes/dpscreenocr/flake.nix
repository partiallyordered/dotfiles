{
  description = "A tool to OCR on-screen text";
  inputs = {
    dpscreenocr = {
      url = "github:danpla/dpscreenocr/master";
      flake = false;
    };
    nixpkgs.url = github:nixos/nixpkgs/nixos-unstable;
  };
  outputs = { self, dpscreenocr, nixpkgs }: {
    packages.x86_64-linux.dpscreenocr = nixpkgs.libsForQt5.callPackage ./. {};
    defaultPackage.x86_64-linux =
      with import nixpkgs { system = "x86_64-linux"; };
      stdenv.mkDerivation {
        name = "dpscreenocr";
        src = dpscreenocr;
        buildPhase = ''
          mkdir build
          cd build
          cmake -G "Unix Makefiles" -DCMAKE_BUILD_TYPE=Release ..
          cd ..
          make
        '';
        buildInputs = with pkgs; [
          cmake
          gettext
          pandoc
          pkg-config
          qt5.qtbase
          qt5.wrapQtAppsHook
          tesseract
          xorg.libX11
          xorg.libXext
        ];
      };
  };
}

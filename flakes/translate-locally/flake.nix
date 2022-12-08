{
  description = "A tool to OCR on-screen text";
  inputs = {
    translateLocally = {
      url = "https://github.com/XapaJIaMnu/translateLocally";
      flake = false;
      submodules = true;
      type = "git";
    };
    nixpkgs = {
      url = github:nixos/nixpkgs/nixos-unstable;
    };
    # TODO: help text if we remove the nixpkgs.config.allowUnfree line is this:
    #   Alternatively you can configure a predicate to allow specific packages:
    # { nixpkgs.config.allowUnfreePredicate = pkg: builtins.elem (lib.getName pkg) [
    #     "mkl"
    #   ];
    # }
    # nixpkgs
  };
  outputs = { self, translateLocally, nixpkgs }: {
    packages.x86_64-linux.translateLocally = nixpkgs.libsForQt5.callPackage ./. {};
    defaultPackage.x86_64-linux =
      with import nixpkgs {
        config.allowUnfreePredicate = pkg: builtins.elem (nixpkgs.lib.getName pkg) [
          "mkl"
        ];
        system = "x86_64-linux";
      };
      stdenv.mkDerivation {
        name = "translateLocally";
        src = translateLocally;
        configurePhase = ''
          echo 'NAME=linux\nVERSION_ID=whatever' > /etc/os-release
        '';
        buildPhase = ''
          echo "STUFF"
          ls 3rd_party/bergamot-translator/3rd_party/
          ls 3rd_party/bergamot-translator/3rd_party/marian-dev/
          echo "STUFF OVER"
          mkdir build
          cd build
          cmake -UUNIX ..
          make
        '';
        buildInputs = with pkgs; [
          # neither of these blas packages appear to contain whatever blas the package is searching
          # for
          blas
          blas-reference
          cmake
          doxygen
          graphviz # for dot, for doc generation
          gperftools # for tcmalloc
          libarchive
          mkl
          qt5.qtbase
          qt5.qttools
          qt5.wrapQtAppsHook
        ];
      };
  };
}

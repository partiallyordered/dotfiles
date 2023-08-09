{ pkgs, lib }:

let
  fontFileName = "Koulen-Regular.ttf";
in
  pkgs.stdenv.mkDerivation rec {
    meta = with lib; {
      homepage = "https://github.com/google/fonts";
      description = "Koulen font";
      platforms = platforms.all;
    };
    version = "47a6c224b3e0287b2e48e3ffef8c9ce2ca4931f4";
    pname = "koulen";
    src = builtins.fetchurl {
      url = "https://github.com/google/fonts/raw/${version}/ofl/koulen/${fontFileName}";
      sha256 = "05q14bjbkk0dj2yg8zlmdszyyp5mmyfv67zzy6q4fxak7jwb06af";
    };
    dontUnpack = true;
    installPhase = ''
      mkdir -p $out/share/fonts/truetype
      cp $src "$out/share/fonts/truetype/${fontFileName}"
    '';
  }

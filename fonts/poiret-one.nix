{ pkgs, lib }:

let
  fontFileName = "PoiretOne-Regular.ttf";
in
  pkgs.stdenv.mkDerivation rec {
    meta = with lib; {
      homepage = "https://github.com/google/fonts";
      description = "Poiret One font";
      platforms = platforms.all;
    };
    version = "47a6c224b3e0287b2e48e3ffef8c9ce2ca4931f4";
    pname = "poiret-one";
    src = builtins.fetchurl {
      url = "https://github.com/google/fonts/raw/${version}/ofl/poiretone/${fontFileName}";
      sha256 = "0n1m59vydhqf6ggypi7rghg5ixnz21xn06ycvfkfan1z4q1jszs5";
    };
    dontUnpack = true;
    installPhase = ''
      mkdir -p $out/share/fonts/truetype
      cp $src "$out/share/fonts/truetype/${fontFileName}"
    '';
  }

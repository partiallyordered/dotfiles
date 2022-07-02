{ pkgs, lib }:

pkgs.stdenv.mkDerivation rec {
  meta = with lib; {
    description = "Steadysets font";
    platforms = platforms.all;
  };
  version = "3232c9222325cf191da3ccaec55c9a4413adb25c";
  pname = "steadysets";
  src = builtins.fetchurl {
    url = "https://github.com/catc/iGrowl/raw/${version}/dist/fonts/steadysets/igrowl-steadysets.ttf";
    sha256 = "0rn9lqj4y6djhxmyxc25z0j64xc38387q2khjr3vjrp3cic12ngx";
  };
  dontUnpack = true;
  installPhase = ''
    mkdir -p $out/share/fonts/truetype
    cp $src $out/share/fonts/truetype/
    '';
}

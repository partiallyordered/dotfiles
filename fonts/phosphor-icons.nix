{ pkgs, lib }:

pkgs.stdenv.mkDerivation rec {
  meta = with lib; {
    description = "Phosphor icons font";
    platforms = platforms.all;
  };
  version = "d3b385a38acbb8b16a068e8e2515660af6978cf9";
  pname = "phosphor-icons";
  src = pkgs.fetchFromGitHub {
    owner = pname;
    repo = "web";
    rev = version;
    sha256 = "0ckrxib0w1c4gw32f5pylrjfz1iinn3qvdbkzzpjgs07h7cg7bnf";
  };
  installPhase = ''
    mkdir -p $out/share/fonts/truetype
    mkdir -p $out/share/fonts/woff2
    mkdir -p $out/share/fonts/woff
    cp $src/src/*/*.ttf $out/share/fonts/truetype/
    cp $src/src/*/*.woff $out/share/fonts/woff/
    cp $src/src/*/*.woff2 $out/share/fonts/woff2/
  '';
}

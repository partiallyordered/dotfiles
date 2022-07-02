{ pkgs, lib }:

pkgs.stdenv.mkDerivation rec {
  meta = with lib; {
    homepage = "https://github.com/isaif/polybar-wifi-ramp-icons";
    description = "A patched \"DejaVu Sans Mono\" font that includes the wifi icons to be used in polybar network ramp-signal";
    license = licenses.mit;
    platforms = platforms.all;
  };
  version = "c88c3a6bf0656e915456688d5a8e938fcc5c5d68";
  pname = "dejavu-wifi-icons";
  src = builtins.fetchurl {
    url = "https://github.com/isaif/polybar-wifi-ramp-icons/raw/${version}/DejaVuSansMono-wifi-ramp.ttf";
    sha256 = "1nq82zcrxp95qcchj2ig998dg63vf0v7pwgqvgv2fnjc3mn4vb5g";
  };
  dontUnpack = true;
  # installPhase = ''
  #   mkdir -p $out/share/fonts/truetype
  #   find -type f -iname '*.ttf' -exec mv {} $out/share/fonts/truetype/ \;
  #   '';
  installPhase = ''
    mkdir -p $out/share/fonts/truetype
    cp $src $out/share/fonts/truetype/
    '';
}

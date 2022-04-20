{ lib, fetchzip }:

let
  version = "c88c3a6bf0656e915456688d5a8e938fcc5c5d68";
in fetchzip {
  name = "dejavu-wifi-icons-${version}";

  url = "https://github.com/isaif/polybar-wifi-ramp-icons/archive/${version}.zip";

  postFetch = ''
    mkdir -p $out/share/fonts
    unzip -j $downloadedFile '*.ttf' -d $out/share/fonts/truetype
  '';

  sha256 = "m0IdymWFUWUXp58jCQh+b28evywFxwdybpm4pTaWUJA=";

  meta = with lib; {
    homepage = "https://github.com/isaif/polybar-wifi-ramp-icons";
    description = "A patched \"DejaVu Sans Mono\" font that includes the wifi icons to be used in polybar network ramp-signal";
    license = licenses.mit;
    platforms = platforms.all;
  };
}

{ lib, fetchzip }:

let
  version = "3232c9222325cf191da3ccaec55c9a4413adb25c";
in fetchzip {
  name = "steadysets-${version}";

  url = "https://github.com/catc/iGrowl/raw/${version}/dist/fonts/steadysets/igrowl-steadysets.ttf";

  postFetch = ''
    mkdir -p $out/share/fonts/truetype/
    mv $downloadedFile $out/share/fonts/truetype/steadysets.ttf
  '';

  sha256 = "H+f7LCBCxvZTFuhk8V+PiwcCoamP+qeA2AAT0Ibe/OU=";

  meta = with lib; {
    description = "Steadysets font";
    platforms = platforms.all;
  };
}

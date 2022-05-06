# { fetchgit
# , stdenv
# , rustPlatform
# }:
# stdenv.mkDerivation {
#   src = fetchgit {
#     url = "git://github.com/facebookincubator/below";
#     rev = "923ed3b5519437007c23ab4ba3cbc1c6ba0205f6";
#     sha256 = "1vlsviza6g5b587439jy7qp5y2xk8smxsl287nk457dyh9g9yf4p";
#   };
#   buildInputs = [  ];
# }
{ fetchFromGitHub
, rustPlatform
, stdenv
}:
rustPlatform.buildRustPackage rec {
  pname = "below";
  version = "923ed3b5519437007c23ab4ba3cbc1c6ba0205f6";

  src = fetchFromGitHub {
    owner = "facebookincubator";
    repo = pname;
    rev = version;
    sha256 = "1iga3320mgi7m853la55xip514a3chqsdi1a1rwv25lr9b1p7vd3";
  };

  cargoSha256 = "17ldqr3asrdcsh4l29m3b5r37r5d0b3npq1lrgjmxb6vlx6a36qh";

  meta = with stdenv.lib; {
    description = "A fast line-oriented regex search tool, similar to ag and ack";
    homepage = "https://github.com/BurntSushi/ripgrep";
    license = licenses.unlicense;
    maintainers = [ maintainers.tailhook ];
  };
}

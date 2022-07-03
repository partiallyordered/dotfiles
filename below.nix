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
, lib
}:
rustPlatform.buildRustPackage rec {
  pname = "below";
  version = "923ed3b5519437007c23ab4ba3cbc1c6ba0205f6";

  src = fetchFromGitHub {
    owner = "facebookincubator";
    repo = pname;
    rev = version;
    hash = "sha256-lzifXoK+nUKmPUhQ3atGswtfLj5epkEOKqs8o37cmu4=";
  };

  cargoSha256 = "08hrq77qf8nh4p81750y8gqgzskzdf0w8ky87i8fnfzf3x7a39aq";

  # pname = "below";
  # version = "0.5.0";
  #
  # src = fetchFromGitHub {
  #   owner = "facebookincubator";
  #   repo = pname;
  #   rev = version;
  #   hash = "sha256-lzifXoK+nUKmPUhQ3atGswtfLj5epkEOKqs8o37cmu4=";
  # };
  # version = "923ed3b5519437007c23ab4ba3cbc1c6ba0205f6";
  #
  # cargoSha256 = "08xiv07yjjrckj87hamj8bar9rhmrc2ksjqgsi5izm0fjz8k0lvz";

  meta = with lib; {
    description = "A time traveling resource monitor for modern Linux systems";
    homepage = "https://github.com/facebookincubator/below";
    license = licenses.asl20;
  };
}

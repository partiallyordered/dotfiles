
#### Hashes

##### Github
```sh
$ git clone --depth 1 https://github.com/owner/repo path/to/clone
$ git -C path/to/clone rev-parse HEAD
65bb66d364e0d10d00bd848a3d35e2755654655b
$ rm -r path/to/clone/.git
$ nix hash-path path/to/clone
sha256-8EUDsWeTeZwJNrtjEsUNLMt9I9mjabPRBZG83u7xtPw=
```

##### Conversion
```sh
$ nix hash to-base32 sha256-8EUDsWeTeZwJNrtjEsUNLMt9I9mjabPRBZG83u7xtPw=
1z5ly7pdxg4i0p8v6sd3v4ipvjrc1p2i4qxv6q4rqyckcyqh6igh
$ nix hash to-base64 sha256:1z5ly7pdxg4i0p8v6sd3v4ipvjrc1p2i4qxv6q4rqyckcyqh6igh
8EUDsWeTeZwJNrtjEsUNLMt9I9mjabPRBZG83u7xtPw=
$ nix hash to-sri sha256:1z5ly7pdxg4i0p8v6sd3v4ipvjrc1p2i4qxv6q4rqyckcyqh6igh
sha256-8EUDsWeTeZwJNrtjEsUNLMt9I9mjabPRBZG83u7xtPw=
```

##### Utils for faking hashes while building a package
```
lib.fakeSha256
lib.fakeSha512
```

#### Licenses
https://github.com/NixOS/nixpkgs/blob/master/lib/licenses.nix

#### callPackage
One way to override a package in nixpkgs is to take the original and modify it:
```sh
curl -L https://github.com/NixOS/nixpkgs/raw/e937d5db28035b20ed3ce43f052cfab4f110a0eb/pkgs/applications/audio/spotify/default.nix > spotify.nix
```
and use it from another nix file, such as `configuration.nix` or (as in this example) `home.nix`:
```nix
{ config, pkgs, lib, ... }:
let
  mySpotify = pkgs.callPackage ./spotify.nix {};
in
{
  home.packages = [ mySpotify ];
}
```

#### Replacing package source
https://nixos.wiki/wiki/Overlays#Overriding_a_version

```nix
kanataHead = pkgs.kanata.overrideAttrs (old: rec {
  version = "fe13389ed81c1c99432766bcc9f528e30ef9da89";
  src = pkgs.fetchFromGitHub {
    owner = "jtroo";
    repo = old.pname;
    rev = version;
    sha256 = "1xr3zmw7mdxs9iziv6v3z5pf5whfwk08rlpsmq48cs1nka4hvn2f";
  };
});
```

#### Wrapping packages
https://nixos.wiki/wiki/Nix_Cookbook#Wrapping_packages

Not strictly wrapping packages, but this allows redefining a derivation:
https://nixos.org/manual/nixpkgs/stable/#sec-pkg-overrideAttrs

##### With a script to e.g. set environment/arguments
This wraps `pkgs.hello` to supply the `-t` argument, and propagates its man pages:
```nix
let
  wrapped = pkgs.writeShellScriptBin "hello" ''
    exec ${pkgs.hello}/bin/hello -t
  '';
in pkgs.symlinkJoin {
  name = "hello";
  paths = [
    wrapped
    pkgs.hello
  ];
}
```


#### Interactively build package
From: https://nixos.wiki/wiki/Nixpkgs/Create_and_debug_packages#Using_nix-shell_for_package_development
```sh
cd $(mktemp -d)
nix-shell -E "with import <nixpkgs> {}; callPackage /path/to/package.nix {}"
```
Now you'll be in a shell with the various builder functions available. You can now run the build
with:
```sh
export out=$PWD/out
source $stdenv/setup
set -x # Optional: it prints all commands, can be practical to debug
set +e # Optional: do not quit the shell on simple errors, Ctrl-C,...
genericBuild
```

#### mkDerivation References
See:
https://nixos.org/manual/nixpkgs/stable/#chap-stdenv
https://nixos.org/manual/nix/stable/#ssec-derivation
https://github.com/NixOS/nixpkgs/issues/18678#issuecomment-569477884
https://nixos.org/manual/nixpkgs/stable/#sec-stdenv-phases

#### Rust
##### Build with openssl, pkgconfig, zlib
From: https://users.rust-lang.org/t/solved-how-do-i-build-cargo-on-nixos/7620/5
`default.nix`:
```nix
with import <nixpkgs> {};
mkShell {
  buildInputs = [ pkgconfig openssl cmake zlib libgit2 ];
  CFG_DISABLE_CROSS_TESTS = "1";
}
```
Inside `nix-shell` run `cargo build` or similar.

#### Haskell
`cabal2nix` example to generate a nix derivation from a cabal project in Github:
```sh
cabal2nix https://github.com/xmonad/xmonad
cabal2nix https://github.com/msk-/xmonad-contrib
```

`cabal2nix` example to generate a nix derivation from a local project. From:
https://haskell4nix.readthedocs.io/nixpkgs-users-guide.html#how-to-create-nix-builds-for-your-own-private-haskell-packages
```sh
cabal2nix . > foo.nix
```
Now create the following:
`default.nix`:
```nix
{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc8103" }:
nixpkgs.pkgs.haskell.packages.${compiler}.callPackage ./foo.nix { }
```
`shell.nix`:
```nix
{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc8103" }:
(import ./default.nix { inherit nixpkgs compiler; }).env
```
Then, to build:
```sh
nix build -L -o foo
```

#### Create a single-file derivation
* Pulls the single file from githubusercontent.com
* Skips the unpack phase
* The sha256 is calculated with `nix-prefetch-url $url`
* Install just copies the source to the destination and sets the execute bit

```nix
# A file preview script for integration with skim/fzf
skimPreviewScript = pkgs.stdenv.mkDerivation rec {
  version = "02a192ea0bed22a015e005b281e55e0da2a8e496";
  pname = "skim-preview-script";
  src = builtins.fetchurl {
    url = "https://raw.githubusercontent.com/junegunn/fzf.vim/02a192ea0bed22a015e005b281e55e0da2a8e496/bin/preview.sh";
    sha256 = "1jjir34n34hp5r6ycy5d8ccs9cqql12g34xy1zhh8lhazvlp30yb";
  };
  dontUnpack = true;
  installPhase = ''
    mkdir -p $out/bin
    cp $src $out/bin/preview.sh
    chmod +x $out/bin/preview.sh
    '';
};
```

##### Create a single-file binary derivation
```nix
tuc = pkgs.stdenv.mkDerivation rec {
  version = "0.10.0";
  pname = "tuc";
  description = "A superset of POSIX cut";
  nativeBuildInputs = [ pkgs.autoPatchelfHook ];
  src = builtins.fetchurl {
    url = "https://github.com/riquito/tuc/releases/download/v${version}/tuc-regex-linux-amd64";
    sha256 = "1wrzdw7ddkla9xnch2vx8jjx2ssz2xnxamis08d04yj45vlg2fs5";
  };
  dontUnpack = true;
  installPhase = ''
    install -m755 -D $src $out/bin/tuc
  '';
};
```

##### Create a derivation from an Ubuntu/Debian .deb package
```nix
translateLocally = pkgs.stdenv.mkDerivation rec {
  version = "v0.0.2+136745e";
  pname = "translateLocally";
  description = "A local-only CLI translator";
  buildInputs = with pkgs; [ libarchive ];
  nativeBuildInputs = with pkgs; [ autoPatchelfHook dpkg qt5.wrapQtAppsHook ];
  src = builtins.fetchurl {
    url = "https://github.com/XapaJIaMnu/translateLocally/releases/download/latest/${pname}-${version}-Ubuntu-20.04.AVX.deb";
    sha256 = "1vzb1lyzyf6263z03ghdb9kk8x3246fjdvl2kv3gr7h4md074hpi";
  };
  unpackPhase = ''
    dpkg -x $src .
  '';
  dontBuild = true;
  installPhase = ''
    install -m755 -D usr/bin/${pname} $out/bin/${pname}
  '';
};
```

##### Fetchzip

When the binary comes zipped, we can use `fetchzip`:
https://github.com/NixOS/nixpkgs/blob/master/pkgs/build-support/fetchzip/default.nix
`fetchzip` is resilient to the zipped file checksum changing, but the content remaining the same.
Make sure to use `nix-prefetch-url --unpack http://path.to/source.tar.gz` to calculate the
checksum.
Note that this example is not building a derivation:
```nix
{ lib, fetchzip }:

let
  version = "c88c3a6bf0656e915456688d5a8e938fcc5c5d68";
in fetchzip {
  name = "dejavu-wifi-icons-${version}";

  url = "https://github.com/isaif/polybar-wifi-ramp-icons/archive/${version}.zip";

  postFetch = ''
    find -type f -iname '*.ttf' -exec mv {} $out/share/fonts/truetype/ \;
  '';

  sha256 = "m0IdymWFUWUXp58jCQh+b28evywFxwdybpm4pTaWUJA=";

  meta = with lib; {
    homepage = "https://github.com/isaif/polybar-wifi-ramp-icons";
    description = "A patched \"DejaVu Sans Mono\" font that includes the wifi icons to be used in polybar network ramp-signal";
    license = licenses.mit;
    platforms = platforms.all;
  };
}
```

#### Find library dependencies
Requires nix-index. Perhaps best installed with home-manager `programs.nix-index.enabled = true;`,
but can be used with `nix-shell -p nix-index`. It will take some time to build an initial index.
```sh
nix-locate --top-level libstdc++.so.6 | grep gcc
```
These should be put into `buildInputs`.

#### Wrapping packages
https://nixos.wiki/wiki/Nix_Cookbook#Wrapping_packages

#### Systemd services in Home Manager
If a package produces systemd files to `$out/lib/systemd/user` they will be installed in the user's
systemd when the package is installed.

##### References
https://nixos.org/patchelf.html
https://nixos.wiki/wiki/Packaging/Binaries
https://brianmckenna.org/blog/running_binaries_on_nixos
https://superuser.com/questions/912389/how-do-i-install-the-32-bit-dynamic-linker-on-64-bit-nixos
https://nixos.wiki/wiki/Nix_Cookbook
https://nixos.wiki/wiki/Nixpkgs/Create_and_debug_packages

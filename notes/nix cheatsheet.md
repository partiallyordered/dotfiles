
### Examine derivations
From a store, e.g. the _zig_ package in the _nixpkgs_ store:
```sh
nix show-derivation 'nixpkgs#zig'
```
See also: https://nixos.org/manual/nix/stable/command-ref/new-cli/nix3-show-derivation.html

It's possible to examine the file structure of the derivation as follows:
```sh
tree $(nix show-derivation 'sys#tree-sitter-grammars.tree-sitter-zig' | jq -r 'to_entries[].value.outputs.out.path')
```

#### Examine a derivation dependency tree
```sh
nix-shell -p nix-tree --command nix-tree "$(readlink -f `which -p viddy`)"
```

### Flakes
Basic flake.nix:
```nix
{
  inputs = {
    nixpkgs.url = github:nixos/nixpkgs/nixos-unstable;
  };
  outputs = { self, nixpkgs }: {
    packages.x86_64-linux.my-package = nixpkgs.callPackage ./. {};
    defaultPackage.x86_64-linux = self.packages.x86_64-linux.my-package;
  };
}
Template projects are available with `nix flake show templates`.
```
Now
1. `git add flake.nix`
2. generate the lock file with `nix flake lock`
3. build the flake with `nix build`

#### Examples/templates
```sh
nix flake show templates
nix flake init -t 'templates#c-hello'
```

#### Run a flake
```sh
$ nix run nixpkgs#rustc -c rustc --version
rustc 1.38.0
$ nix run nixpkgs/release-19.09#rustc -c rustc --version
rustc 1.37.0
$ nix run patchelf -c patchelf --version
patchelf 0.10.20191023.2ba6481
$ nix run github:edolstra/dwarffs/50023d28e814... -c ...
```
Source: https://edolstra.github.io/talks/nixcon-oct-2019.pdf

#### Update flake
Update all inputs:
```sh
nix flake update
```
Update single input:
```sh
nix flake lock --update-input home-manager
```

#### Example flake that builds a non-flake repo
This example exists with a lockfile, and was tested at the revision this note was created.
1. Create this file as `flake.nix` in a directory
2. Use `nix build` to build it
3. Run `./result/bin/dpscreenocr`
```nix
{
  description = "A tool to OCR on-screen text";
  inputs = {
    dpscreenocr = {
      url = "github:danpla/dpscreenocr/master";
      flake = false;
    };
    nixpkgs.url = github:nixos/nixpkgs/nixos-unstable;
  };
  outputs = { self, dpscreenocr, nixpkgs }: {
    # For non-qt applications, we'd just call callPackage:
    # packages.x86_64-linux.dpscreenocr = nixpkgs.callPackage ./. {};
    packages.x86_64-linux.dpscreenocr = nixpkgs.libsForQt5.callPackage ./. {};
    defaultPackage.x86_64-linux =
      with import nixpkgs { system = "x86_64-linux"; };
      stdenv.mkDerivation {
        name = "dpscreenocr";
        src = dpscreenocr;
        buildPhase = ''
          mkdir build
          cd build
          cmake -G "Unix Makefiles" -DCMAKE_BUILD_TYPE=Release ..
          cd ..
          make
        '';
        buildInputs = with pkgs; [
          cmake
          gettext
          pandoc
          pkg-config
          qt5.qtbase
          qt5.wrapQtAppsHook
          tesseract
          xorg.libX11
          xorg.libXext
        ];
      };
  };
}
```

#### Unfree flake inputs
Example allowing the unfree `mkl` package. Note that the nixpkgs config is in the outputs, not the
inputs:
```nix
{
  inputs = {
    nixpkgs.url = github:nixos/nixpkgs/nixos-unstable;
  };
  outputs = { self, nixpkgs }: {
    packages.x86_64-linux.my-package = nixpkgs.callPackage ./. {};
    defaultPackage.x86_64-linux = with import nixpkgs {
      config.allowUnfreePredicate = pkg: builtins.elem (nixpkgs.lib.getName pkg) [
        "mkl"
      ];
    }; self.packages.x86_64-linux.my-package;
  };
}
```

### Search configuration
```sh
man configuration.nix
man home-configuration.nix
```
or https://search.nixos.org/options

### Install nix on running Ubuntu docker image
```sh
# install prerequisites for the script
apt-get update && apt-get install -y xz-utils curl
NEW_USER="gandalf"
# create the user
useradd -m $NEW_USER
# create the nix store directory, owned by the new user's user/group
install -d -m755 -o $NEW_USER -g $NEW_USER /nix
# enter a login shell as the new user
su -l $NEW_USER
# single-user nix install
curl -L https://nixos.org/nix/install | sh
# bring nix and installed packages into PATH
. $HOME/.nix-profile/etc/profile.d/nix.sh
# if you need to log out of $NEW_USER, be sure to log in again with `su -l $NEW_USER` to ensure
# .profile etc. are sourced
```

### Node packages
https://github.com/NixOS/nixpkgs/blob/master/pkgs/development/node-packages/node-packages.json

### Serve local binary cache
Refs:
- https://nixos.org/manual/nix/stable/package-management/binary-cache-substituter.html
- https://nixos.wiki/wiki/Binary_Cache#Using_a_binary_cache

Temporary use of the cache:
```sh
nixos-rebuild switch --option substituters http://binarycache.example.com
# this might also work:
nixos-rebuild switch --option binary-caches "http://binarycache.example.com"
```

### Clean up
Reference: https://nixos.wiki/wiki/Storage_optimization

#### Clear out stuff older than a certain date
E.g. 30 days:
```sh
nix-collect-garbage --delete-older-than 30d
```

#### Optimise store
```sh
nix-store --optimise
```

#### `result` symlinks
Look for result symlinks (system-wide, in this case):
```sh
nix-store --gc --print-roots
```
```sh
find / -type l -name result
```
Delete any that aren't useful then run nix-collect-garbage.

#### Old generations
List generations:
```sh
sudo nix-env -p /nix/var/nix/profiles/system --list-generations
```
Remove all but the current generation:
```sh
sudo nix-collect-garbage -d
```
List home manager generations:
```sh
home-manager generations
```
Remove all but the current home-manager generation (note the lack of `sudo`):
```sh
nix-collect-garbage -d
```

### Roll back NixOS
To the last generation:
```sh
nixos-rebuild switch --rollback
```
To a specific generation:
```sh
GENERATION=12345
sudo nix-env --switch-generation "$GENERATION" -p /nix/var/nix/profiles/system
sudo /nix/var/nix/profiles/system/bin/switch-to-configuration switch
```
See also: https://github.com/NixOS/nixpkgs/issues/24374#issuecomment-738712420

### List generations
System profile:
```sh
nix-env --list-generations --profile /nix/var/nix/profiles/system
```
Active profile:
```sh
nix-env --list-generations
```

### Remove specific package/derivation
```sh
nix-store --delete /nix/store/path/to/package
```
You may receive a message like:
```
error: Cannot delete path '/nix/store/path/to/package' since it is still alive. To find out why, use: nix-store --query --roots
```
If you run `nix-store --query --roots /nix/store/path/to/package` you'll eventually find that your
package is still alive due to a specific generation. Follow the steps in [Remove specific
generations](#remove-specific-generations).

### Remove specific generation
First, list generations, check the profile you're going to delete is not `(current)`:
```sh
nix-env --list-generations --profile /nix/var/nix/profiles/system
```
Say you'd like to remove profile 273, that's stored at `/nix/var/nix/profiles/system-273-link`. You
need simply to:
```sh
rm /nix/var/nix/profiles/system-273-link
```

### Nix rebuild offline (without internet)
```sh
sudo nixos-rebuild switch --option binary-caches ""
```

### Enter REPL:
```sh
nix repl
```

### List imperatively installed packages
```sh
nix-env -q
```
Remove them:
```sh
nix-env -e package-name
```

### Find package:
```sh
nix-env -qaP [package-name]
```
or, faster:
```sh
nix-env -f '<nixpkgs>' -qaP -A [package-name]
```
Modern, search the nixpkgs flake for blender:
```sh
nix search nixpkgs blender
```
Or, using https://discourse.nixos.org/t/local-flake-based-nix-search-nix-run-and-nix-shell/13433/4
```sh
nix search sys blender
```

### Find a specific binary:
Try it:
```sh
$ neofetch
The program 'neofetch' is not in your PATH. You can make it available in an
ephemeral shell by typing:
  nix-shell -p neofetch
```
Or (this takes a while if the index isn't built):
```sh
nix-shell -p nix-index --run 'nix-index && nix-locate neofetch'
```

### Start shell with specific packages installed:
```sh
nix-shell -p kubectl kubernetes-helm socat jq
```

### System upgrade:
```sh
nixos-rebuild switch --upgrade
```
Flakes:
```sh
sudo nixos-rebuild switch --flake ~/.dotfiles/
```

### List files installed by package:
```sh
tree $(dirname $(dirname $(readlink -f $(which vim))))
```
Slightly more generally (from https://nixos.wiki/wiki/Cheatsheet ):
```sh
$ readlink -f $(which vim)
 /nix/store/ji06y4haijly0i0knmr986l2dajffv1p-emacs-24.4/bin/emacs-24.4
```
then
```sh
du -a /nix/store/ji06y4haijly0i0knmr986l2dajffv1p-emacs-24.4
```

### Temporarily install package
```sh
nix-env -iA nixos.sxiv
# when finished
nix-env -e sxiv # don't know why this needs to be unqualified
```

### Get hash of input from URL:
```sh
nix-prefetch-url mirror://pypi/g/grpclib/grpclib-0.3.1.tar.gz
```
For git (you'll need to install nix-prefetch-git, perhaps in a `nix-shell -p nix-prefetch-git`
environment):
```sh
nix-prefetch-git https://github.com/lstwn/broot.vim
```
Or for github, using `nix-prefetch-url`, note `--unpack`:
```sh
nix-prefetch-url --unpack https://github.com/cstrahan/vim-capnp/archive/954202e2c6c1cb9185082de8ddb7f2823a9d1206.tar.gz
```

### Trace:
```nix
{ a, b ? 3, trueMsg ? "yes", falseMsg ? "no" }:
if a > b
  then builtins.trace trueMsg true
  else builtins.trace falseMsg false
```

### Share dependencies with others
`default.nix`:
```nix
# pin with e.g.
#   { pkgs ? import (fetchTarball "https://github.com/NixOS/nixpkgs/archive/3590f02e7d5760e52072c1a729ee2250b5560746.tar.gz")) {} }:
# see also: https://nixos.org/guides/towards-reproducibility-pinning-nixpkgs.html
{ pkgs ? import <nixpkgs> {  } }:

let kubefwd = pkgs.stdenv.mkDerivation rec {
  version = "1.18.1";
  pname = "kubefwd";

  src = builtins.fetchurl {
    url = "https://github.com/txn2/kubefwd/releases/download/${version}/kubefwd_Linux_x86_64.tar.gz";
    sha256 = "1864jayfczlyliz6h7gybrjrfnyabshc0kpbxvavp6ni9r6pm489";
  };

  unpackPhase = ''
    tar xf $src
  '';

  installPhase = ''
      install -m755 -D kubefwd $out/bin/kubefwd
  '';
};

in

[
  pkgs.terraform_0_14
  pkgs.python39Packages.jsonschema
  pkgs.jq
  kubefwd
]
```

`shell.nix`:
```nix
# pin with e.g.
#   { pkgs ? import (fetchTarball "https://github.com/NixOS/nixpkgs/archive/3590f02e7d5760e52072c1a729ee2250b5560746.tar.gz")) {} }:
# see also: https://nixos.org/guides/towards-reproducibility-pinning-nixpkgs.html
{ pkgs ? import <nixpkgs> {} }:
pkgs.mkShell {
  nativeBuildInputs = import ./default.nix { inherit pkgs; };
}
```

Now anyone with `nix` can run `nix-shell shell.nix` to get a shell containing the same packages as
the author.

### Create more portable scripts
Here we start the script by using `nix-shell` as the script interpreter. Then we immediately run a
bash shell with curl preinstalled.
```sh
#!/usr/bin/env nix-shell
#!nix-shell -i bash -p curl -I nixpkgs=https://github.com/NixOS/nixpkgs/archive/8a3eea054838b55aca962c3fbde9c83c102b8bf2.tar.gz
set -euo pipefail
curl -L localhost:3000
```
Remove `-I nixpkgs=...` from the second line to use the system nixpkgs.

### Deprecated openjdk versions
```nix
# This file largely derived from:
# https://github.com/NixOS/nixpkgs/blob/5d380b2f7e60a28ea06cfebce6beb76ca3ff22d1/pkgs/development/compilers/openjdk/13.nix
# At the time of writing, an attempt was made to build from that, but the build failed. Instead,
# this attempt at using the prebuilt, archived openjdk distribution was made. Some alternative
# possibilities may have been:
# - use a version of nixpkgs from an earlier date
# - find a prebuilt jdk in a nix binary cache (e.g. cachix)

# nixpkgs-unstable HEAD at the time of writing
{ pkgs ? import (fetchTarball "https://github.com/NixOS/nixpkgs/archive/103a4c0ae46afa9cf008c30744175315ca38e9f9.tar.gz") {} }:

let
  openjdk13 = with pkgs; stdenv.mkDerivation rec {
    version = "13.0.2";
    pname = "openjdk13";
    description = "Java development kit";

    installPhase = ''
      mkdir -p $out/lib/openjdk

      mv bin conf include jmods legal lib $out/lib/openjdk

      # Remove some broken manpages.
      rm -rf $out/lib/openjdk/man/ja*

      # Mirror some stuff in top-level.
      mkdir -p $out/share
      ln -s $out/lib/openjdk/include $out/include
      ln -s $out/lib/openjdk/man $out/share/man

      # jni.h expects jni_md.h to be in the header search path.
      ln -s $out/include/linux/*_md.h $out/include/

      # Remove crap from the installation.
      rm -rf $out/lib/openjdk/demo

      ln -s $out/lib/openjdk/bin $out/bin
    '';

    preFixup = ''
      # Propagate the setJavaClassPath setup hook so that any package
      # that depends on the JDK has $CLASSPATH set up properly.
      mkdir -p $out/nix-support
      #TODO or printWords?  cf https://github.com/NixOS/nixpkgs/pull/27427#issuecomment-317293040
      echo -n "${setJavaClassPath}" > $out/nix-support/propagated-build-inputs

      # Set JAVA_HOME automatically.
      mkdir -p $out/nix-support
      cat <<EOF > $out/nix-support/setup-hook
      if [ -z "\''${JAVA_HOME-}" ]; then export JAVA_HOME=$out/lib/openjdk; fi
      EOF
    '';

    postFixup = ''
      # Build the set of output library directories to rpath against
      LIBDIRS=""
      for output in $outputs; do
        if [ "$output" = debug ]; then continue; fi
        LIBDIRS="$(find $(eval echo \$$output) -name \*.so\* -exec dirname {} \+ | sort | uniq | tr '\n' ':'):$LIBDIRS"
      done
      # Add the local library paths to remove dependencies on the bootstrap
      for output in $outputs; do
        if [ "$output" = debug ]; then continue; fi
        OUTPUTDIR=$(eval echo \$$output)
        BINLIBS=$(find $OUTPUTDIR/bin/ -type f; find $OUTPUTDIR -name \*.so\*)
        echo "$BINLIBS" | while read i; do
          patchelf --set-rpath "$LIBDIRS:$(patchelf --print-rpath "$i")" "$i" || true
          patchelf --shrink-rpath "$i" || true
        done
      done
    '';

    buildInputs = with pkgs; [
      cpio file which zip perl zlib cups freetype alsa-lib libjpeg giflib libpng zlib lcms2
      xorg.libX11 xorg.libICE xorg.libXrender xorg.libXext xorg.libXtst xorg.libXt xorg.libXtst
      xorg.libXi xorg.libXinerama xorg.libXcursor xorg.libXrandr fontconfig
    ];

    nativeBuildInputs = [ pkgs.autoPatchelfHook ];
    src = builtins.fetchurl {
      # Prebuilt jdk available at https://jdk.java.net/archive/
      url = "https://download.java.net/java/GA/jdk${version}/d4173c853231432d94f001e99d882ca7/8/GPL/openjdk-${version}_linux-x64_bin.tar.gz";
      sha256 = "acc7a6aabced44e62ec3b83e3b5959df2b1aa6b3d610d58ee45f0c21a7821a71";
    };

    dontBuild = true;
  };

in pkgs.mkShell {
  buildInputs = with pkgs; [
    openjdk13
  ];
}
```

### Links
https://nixos.wiki/wiki/Packaging/Examples

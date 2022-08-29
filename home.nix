{ config, pkgs, lib, ... }:
let
  myDsq = pkgs.stdenv.mkDerivation rec {
    version = "0.20.1";
    pname = "dsq";
    description = "Query structured data files with SQL";
    nativeBuildInputs = [ pkgs.autoPatchelfHook pkgs.unzip ];
    src = builtins.fetchurl {
      url = "https://github.com/multiprocessio/dsq/releases/download/0.20.1/dsq-linux-x64-${version}.zip";
      sha256 = "0aip8cl87xdaickq56c1j55r9hmk9wwghipls12rypmny31x0d19";
    };
    installPhase = ''
      install -m755 -D dsq $out/bin/dsq
    '';
    sourceRoot = ".";
  };

  filesIn = with lib; with builtins; dir: suffix:
    foldl
      (a: b: a + "\n" + b)
      ""
      (attrValues
          (mapAttrs
            (name: _: readFile (dir + "/${name}"))
            (filterAttrs (name: type: hasSuffix ".${suffix}" name && type == "regular") (readDir dir))));

  # To add to this, add packages of interest to node-packages.json, then run
  # `node2nix -10 -i node-packages.json`
  # (probably change the node version)
  # `home-manager switch`
  myNode = pkgs.nodejs-16_x;
  # myNodePackages = import ./node/default.nix {
  #   nodejs = myNode;
  # };

  basicService = { desc, cmd, env ? "" }:
    {
        Unit = {
          Description = desc;
          # TODO: See man-home-configuration.nix for systemd.user.targets
          After = [ "network-online.target" ];
        };

        Service = {
          ExecStart = cmd;
          KillSignal = "SIGTERM";
          TimeoutStopSec = 5;
          Environment = env;
        };
    };

  constrainedService = { cmd, cpu ? "100%", mem ? "1G", desc ? "", env ? "" }:
    let s = basicService { desc = desc; cmd = cmd; env = env; };
    # TODO: is there a way to "deep-replace" below instead of having the awkward .Service
    # replacement?
    in
      s // {
        Service = s.Service // {
          CPUQuota = cpu;
          MemoryMax = mem;
        };
      };

  firefoxService = { name, desc, url, env ? "", profile ? name }:
    constrainedService {
      inherit desc;
      cpu = "150%";
      mem = "2G";
      # https://wiki.archlinux.org/title/Firefox#Touchscreen_gestures_and_pixel-perfect_trackpad_scrolling
      env = "${env} MOZ_USE_XINPUT2=1";
      # For some command-line options see:
      # - https://docs.gtk.org/gtk3/running.html
      # - https://docs.gtk.org/gtk3/x11.html
      # - https://wiki.mozilla.org/Firefox/CommandLineOptions
      # hard-coding https means things won't work for non-https URLs
      cmd = "${pkgs.firefox}/bin/firefox --no-remote --class=${name} -P ${profile} https://${url}";
    };

  customVimPlugins = {
    vim-capnp = pkgs.vimUtils.buildVimPlugin {
      name = "vim-capnp";
      src = pkgs.fetchFromGitHub {
        owner = "cstrahan";
        repo = "vim-capnp";
        rev = "954202e2c6c1cb9185082de8ddb7f2823a9d1206";
        sha256 = "02nwxibfq1ddl3idms29c73b06rc5gpimdasfnn4pdafd7mhil7a";
      };
    };
    vim-yaml-folds = pkgs.vimUtils.buildVimPlugin {
      name = "vim-yaml-folds";
      src = pkgs.fetchFromGitHub {
        owner = "pedrohdz";
        repo = "vim-yaml-folds";
        rev = "890ccd8e5370808d569e96dbb06cbeca2cf5993a";
        sha256 = "018z6xcwrq58q6lj6gwhrifjaxkmrlkkg0n86s6mjjlwkbs2qa4m";
      };
    };
    # Plugin 'https://github.com/mxw/vim-jsx'
    # Plugin 'https://github.com/kana/vim-textobj-user'
  };

  customZshPlugins = [
    {
      name = "zsh-syntax-highlighting";
      src = pkgs.fetchFromGitHub {
        owner = "zsh-users";
        repo = "zsh-syntax-highlighting";
        rev = "e900ad8bad53501689afcb050456400d7a8466e5";
        sha256 = "1dfy5wvkmnp2zzk81fhc7qlywgn0j6z0vjch5ak5r3j2kqv61cmi";
      };
    }
    {
      # will source zsh-autosuggestions.plugin.zsh
      name = "zsh-autosuggestions";
      src = pkgs.fetchFromGitHub {
        owner = "zsh-users";
        repo = "zsh-autosuggestions";
        rev = "a7f0106b31c2538a36cab30428e6ca65d9a2ae60";
        sha256 = "0z6i9wjjklb4lvr7zjhbphibsyx51psv50gm07mbb0kj9058j6kc";
      };
    }
    # {
    #   name = "zsh-fzy";
    #   src = pkgs.fetchFromGitHub {
    #     owner = "aperezdc";
    #     repo = "zsh-fzy";
    #     rev = "5d54f3927529b8d8a105376a3b51e51bb3fa3ca2";
    #     sha256 = "1yncmcsyz4ch9i57cvix1hsl9915r7sj0vffbx1q3dsv9n6x3wgn";
    #   };
    # }
  ];

  userTempDirName = ".tmpfiles";
  userScriptDir = ".local/bin";

  firefox = import ./firefox.nix { inherit config pkgs lib; };
  work = import ./work.nix;

in
{
  programs.home-manager.enable = true;
  programs.home-manager.path = https://github.com/rycee/home-manager/archive/master.tar.gz;

  home.username = "msk";
  home.homeDirectory = "/home/msk";

  # This value determines the Home Manager release that your
  # configuration is compatible with. This helps avoid breakage
  # when a new Home Manager release introduces backwards
  # incompatible changes.
  #
  # You can update Home Manager without changing this value. See
  # the Home Manager release notes for a list of state version
  # changes in each release.
  home.stateVersion = "21.05";

  # TODO: gtk theme doesn't seem to be working. How to test?
  gtk.theme = {
    package = pkgs.layan-gtk-theme;
    name = "Layan";
  };

  # TODO:
  # programs.mpv = { }..
  programs.starship = {
    enable = true;
    settings = {
      # Use `starship explain` to explain what's visible
      format = lib.concatStrings [
        "$time"
        "$username"
        "$hostname"
        "$localip"
        "$shlvl"
        "$kubernetes"
        "$directory"
        "$git_branch"
        "$git_commit"
        "$git_state"
        "$git_metrics"
        "$git_status"
        "$terraform"
        "$nix_shell"
        "$gcloud"
        "$env_var"
        "$jobs"
        "$cmd_duration"
        "$line_break"
        "$battery"
        "$status"
        "$shell"
        "$character"
      ];
      jobs = {
        number_threshold = 1;
        symbol = "jobs: ";
      };
      time = {
        disabled = false;
        format = "[$time]($style) ";
      };
      status = {
        disabled = false;
        success_symbol = "✅";
      };
      directory = {
        truncation_length = 4;
      };
    };
  };

  programs.firefox = firefox;
  imports = [
    ./polybar.nix
  ];

  programs.direnv.enable = true;

  programs.nushell = {
    enable = true;
  };

  programs.lazygit = {
    enable = true;
    settings = {
      disableStartupPopups = true;
      promptToReturnFromSubprocess = false;
    };
  };

  programs.feh.enable = true;
  programs.htop.enable = true; # TODO: check out the config options you didn't know were there
  programs.chromium = {
    enable = true;
    extensions = [
      # TODO: privacy badger?
      # TODO: videostream?
      "lmhkpmbekcpmknklioeibfkpmmfibljd" # redux dev tools
      "fmkadmapgofadopljbjfkapdkoienihi" # react dev tools
      "cjpalhdlnbpafiamejdnhcphjbkeiagm" # ublock origin
      "gcbommkclmclpchllfjekcdonpmejbdp" # https everywhere
      "ldpochfccmkkmhdbclfhpagapcfdljkj" # decentraleyes: remember to search 'https everywhere decentraleyes'
      "jcgpghgjhhahcefnfpbncdmhhddedhnk" # click to remove element
      # "flnagcobkfofedknnnmofijmmkbgfamf" # url tracking and redirect skipper
      "kcpnkledgcbobhkgimpbmejgockkplob" # tracking token skipper
      "jaoafjdoijdconemdmodhbfpianehlon" # skip redirect
      "nomnklagbgmgghhjidfhnoelnjfndfpd" # canvas blocker
      "naepdomgkenhinolocfifgehidddafch" # browserpass
    ];
  };
  programs.browserpass = {
    enable = true;
    browsers = [ "firefox" "chromium" ];
  };

  # TODO: some of these files should be in xdg.configFile or xdg.dataFile. Which ones? Perhaps all?
  # TODO: these scripts should be derivations so they can be used outside of my user shell, e.g. in
  #       systemd services
  home.file =
    # TODO:
    # Many of these bash scripts could be single-line aliases, but I want to make them available
    # outside the shell. In particular, at the time of writing
    # - in rofi, with shift+enter to open them in a short-lived terminal instance
    # - from polybar
    # Also, investigate/consider using this: https://github.com/NixOS/nixpkgs/blob/755d7a0735d25192f647b5f85c50d3faf22fccb2/pkgs/build-support/trivial-builders.nix#L253-L274
    # Also.. rewrite these using Haskell and conduit or something..? And just build them as part of
    # the process of building the system? Make a nice template that optionally handles stdin and
    # command-line parameters etc.?
    # Also, investigate/consider making all of these available as flakes
    let bashScript = { text, name }: {
        text = ''
          #!${pkgs.bash}/bin/bash
          set -euo pipefail
          ${text}
        '';
        executable = true;
        target = "${userScriptDir}/${name}";
      };

      clipArgsName             = "clip-args";
      selectFirefoxProfileName = "select-firefox-profile";
      firefoxAppName           = "firefox-app";
      chromiumThrowawayName    = "chromium-throwaway";
      chromiumDevName          = "chromium-insecure-dev";

      home      = "${config.home.homeDirectory}";
      dots      = "${home}/.dotfiles";

      awk       = "${pkgs.gawk}/bin/awk";
      broot     = "${pkgs.broot}/bin/broot";
      column    = "${pkgs.util-linux}/bin/column";
      grep      = "${pkgs.gnugrep}/bin/grep";
      jq        = "${pkgs.jq}/bin/jq";
      mktemp    = "${pkgs.coreutils-full}/bin/mktemp";
      notify    = "${pkgs.libnotify}/bin/notify-send";
      rofi      = "${pkgs.rofi}/bin/rofi";
      sed       = "${pkgs.gnused}/bin/sed";
      shell     = "${pkgs.zsh}/bin/zsh";
      systemctl = "${pkgs.systemd}/bin/systemctl";
      tr        = "${pkgs.coreutils-full}/bin/tr";
      terminal  = "${pkgs.alacritty}/bin/alacritty";
      xclip     = "${pkgs.xclip}/bin/xclip";
      zenity    = "${pkgs.gnome.zenity}/bin/zenity";
    in
    {
      hold = bashScript {
        text = ''
          # Hold open whatever is passed in. E.g.
          #   hold ls -hAl
          # Useful for holding a terminal window open after running an ephemeral command
          # Allow errors:
          set +e
          "$@"
          read -n1
          '';
        name = "hold";
      };
      contains-element = bashScript {
        text = ''
          set -euo pipefail
          match="$1"
          shift
          for e; do [[ "$e" == "$match" ]] && exit 0; done
          exit 1
          '';
          name = "contains-element";
      };
      translate-x11-primary-selection = bashScript {
        text = ''
          set -euo pipefail
          SELECTION=primary
          while [[ $# > 0 ]]
          do
              param_name="$1"
              shift
              case $param_name in
                  -s|--selection|--sel)
                      SELECTION="$1"
                      if ! ${config.home.homeDirectory}/${config.home.file.contains-element.target} "$SELECTION" "clipboard" "primary" "secondary"; then
                        echo "Error: invalid xclip selection type supplied to \"$param_name\": \"$SELECTION\". Allowed: clipboard, primary, secondary."
                        exit 1
                      fi
                      shift
                      ;;
                  *)
                      echo "Unrecognised parameter"
                      usage
                      exit 1
                      ;;
              esac
          done
          TEXT="$(${xclip} -selection "$SELECTION" -o)"
          crow "$TEXT"
        '';
        name = "translate-x11-primary-selection";
      };
      type-clipboard = bashScript {
        text = let xdotool = "${pkgs.xdotool}/bin/xdotool"; in ''
          set -euo pipefail
          SELECTION="$(echo -e 'clipboard\nprimary\nsecondary' | ${rofi} -dmenu -no-custom -i -p '> ')"
          TEXT="$(${xclip} -selection \"$SELECTION\" -o)"
          if ${zenity} --question --text "Type this?\n$TEXT"; then
            ${xdotool} type "$TEXT"
          fi
        '';
        name = "type-clipboard";
      };
      edot = bashScript { text = "${broot} -i -h ${dots}/"; name = "edot"; };
      tv = bashScript { text = "${broot} -i -h ${dots}/notes"; name = "tv"; };
      notes = bashScript {
        text = "${broot} $HOME/projects/github.com/msk-/turbo-computing-machine";
        name = "notes";
      };
      bt-conn = bashScript {
        text = let btctl = "${pkgs.bluez}/bin/bluetoothctl"; in ''
          ACTION=$(echo -e 'connect\ndisconnect' | ${rofi} -dmenu -no-custom -i -p '> ')
          DEVICE=$(${btctl} devices | cut -f2- -d' ' | ${rofi} -dmenu -no-custom -i -p '> ' | cut -f1 -d' ')
          ${btctl} $ACTION $DEVICE
        '';
        name = "bt-conn";
      };
      clip = bashScript {
        text = ''
          shopt -s lastpipe
          ${xclip} -selection primary -filter |\
            ${xclip} -selection secondary -filter |\
            ${xclip} -selection clipboard -filter |\
            read COPIED
          echo "Copied \"$COPIED\" to clipboard."
        '';
        name = "clip";
      };
      ${clipArgsName} = bashScript {
        text = ''echo "$@" | ${home}/${config.home.file.clip.target}'';
        name = clipArgsName;
      };
      ${selectFirefoxProfileName} = bashScript {
        # TODO: Can we have autocomplete arguments?
        #       Can we use xmonadprompt or rofi to autocomplete them?
        #       Can we supply them to XDG stuff? Can the corresponding rofi menu support autocomplete?
        #       https://askubuntu.com/questions/68175/how-to-create-script-with-auto-complete
        # TODO: set selected to "default" profile rather than just row zero?
        text = ''
          PROFILES="${builtins.concatStringsSep "\n" (builtins.attrNames firefox.profiles)}"
          SELECTED=$(echo -e "$PROFILES" | ${rofi} -dmenu -p '> ' -no-custom -i -selected-row 0)
          firefox -P "$SELECTED" "$@"
          '';
          name = selectFirefoxProfileName;
      };
      select-browser = bashScript {
        # TODO: for this menu to be "nice" we can't refer to the packages here and therefore
        # require them using nix. Ideally we should do one of the following
        # - map strings to browsers in this script
        # - put all browser scripts in a ~/.local/bin/browser directory or similar, then just
        #   display the contents of that directory in this script, for the user to select from
        text = ''
          BROWSERS="${firefoxAppName}\n${chromiumDevName}\n${chromiumThrowawayName}\nchromium\n${selectFirefoxProfileName}\nchromium --incognito\nfirefox --private-window\n${pkgs.surf}/bin/surf\nclip-args\nfreetube"
          SELECTED=$(echo -e "$BROWSERS" | ${rofi} -dmenu -p '> ' -no-custom -i -selected-row 0)
          $SELECTED "$@"
        '';
        name = "select-browser";
      };
      mktempdir = bashScript {
        text = ''
          DIR="$(${mktemp} -d --tmpdir=${home}/${userTempDirName} "$@")"
          echo -n "$DIR"
        '';
        name = "mktempdir";
      };
      ${firefoxAppName} = bashScript {
        text = ''${pkgs.firefox}/bin/firefox -P app --class app --new-window "$@"''; name = firefoxAppName;
      };
      # TODO: put the various browser options into xdg desktop things, so it's easier to go
      # straight to them?
      ${chromiumThrowawayName} = bashScript {
        text = ''
          TEMP_PROFILE_DIR=$(${home}/${config.home.file.mktempdir.target})
          ${pkgs.chromium}/bin/chromium --incognito --class=app --user-data-dir=$TEMP_PROFILE_DIR "$@"
          rm -rf $TEMP_PROFILE_DIR
        '';
        name = chromiumThrowawayName;
      };
      ${chromiumDevName} = bashScript {
        # Derived from the following SO answer, which seems thus far to be kept updated
        # https://stackoverflow.com/a/58658101
        text = ''
          ${pkgs.chromium}/bin/chromium --disable-web-security --disable-site-isolation-trials --user-data-dir=$XDG_CONFIG_HOME/.config/${chromiumDevName} "$@";
        '';
        name = chromiumDevName;
      };
      update = bashScript {
        # TODO: can we modify the "update" notification provided by notify-send to activate a
        # specific workspace + window? Or perhaps if we've integrated the update functionality with
        # pueue, we could pop up a terminal displaying the result
        # TODO: we use --use-remote-sudo because of https://github.com/NixOS/nixpkgs/issues/169193
        # TODO: can we set our window to urgent once this is complete? Then our workspace/window
        # title can be highlighted by XMonad.
        #
        # Set the urgency hint given a window ID:
        #   xdotool set_window --urgency 0 0x0260002c
        # In bash, zsh get the parent process ID (will be the terminal in a shell run in terminal,
        # but in a multiplexed terminal client, this will be the terminal server PID):
        #   echo $PPID
        # Get the Alacritty process ID:
        #   echo ${ALACRITTY_LOG//*(\/tmp\/Alacritty-|.log)/}
        # Get a window ID from a process ID (see the other search terms also):
        #   xdotool search --pid 3719679
        # Get a window ID:
        #   xwininfo
        # Get window properties:
        #   xprop
        # Watch/tail window property changes:
        #   xprop -spy
        # In most terminals, set the visual bell (and normally, correspondingly, the X11 urgency
        # hint):
        #   echo '\a'
        # Issues in wezterm for urgency hints:
        # - https://github.com/wez/wezterm/pull/1636
        # - https://github.com/wez/wezterm/issues/1789
        text = ''
          trap '${notify} "Update failed"' ERR
          ${pkgs.nixos-rebuild}/bin/nixos-rebuild --use-remote-sudo switch --flake ${home}/.dotfiles/ "$@"
          ${notify} 'Updated'
        '';
        name = "update";
      };
      makes_tempfile_directory = {
        text = ''
          This file is created by home manager. It is a placeholder that causes home manager to
          create the directory containing this file. This directory is used to host temporary files,
          in order to distinguish temporary files created by the user from temporary files created
          elsewhere.
        '';
        executable = true;
        target = "${userTempDirName}/dummy";
      };
      invalidategpgcacheonscreenlock = bashScript {
        text = ''
          ${pkgs.gnupg}/bin/gpg-connect-agent reloadagent \bye
          ${pkgs.xsecurelock}/bin/xsecurelock
        '';
        name = "invalidate_gpg_cache_on_screen_lock";
      };
      yamllint = {
        source = ./yamllint/config.yaml;
        target = ".config/yamllint/config";
      };
      alacrittyConf = {
        source = ./alacritty.yml;
        target = ".config/alacritty/alacritty.yml";
      };
      ultisnipsKubernetesSnippets = {
        source = ./ultisnips;
        target = ".config/nvim/UltiSnips";
      };
      select-mullvad-country =
        let
          mullvad = "${pkgs.mullvad}/bin/mullvad";
        in bashScript {
          text =
          ''
            #!${pkgs.bash}/bin/bash
            ${mullvad} relay set location $( \
              ${mullvad} relay list | \
              ${grep} '^\S' | \
              ${rofi} -no-custom -dmenu -p '> ' -i | \
              ${sed} 's/^[^(]*(\(.*\))$/\1/g')
          '';
          name = "select-mullvad-country";
        };
      prnotify =
        let
          hub = "${pkgs.hub}/bin/hub";
          sleep = "${pkgs.coreutils-full}/bin/sleep";
        in bashScript {
          name = "prnotify";
          text = ''
            if [[ $(${hub} ci-status) == "no status" ]]; then
                echo "No PR status yet. Waiting ten seconds then trying again."
                ${sleep} 10
            fi

            if [[ $(${hub} ci-status) == "pending" ]]; then
                while [[ $(${hub} ci-status) == "pending" ]]; do ${sleep} 20; done
            fi

            # notify user
            pr_status="$(${hub} ci-status)"
            msg="Github PR status for directory $PWD : $pr_status. PR link copied to clipboard."
            echo "$msg"
            ${notify} "$msg"
            # copy PR link to clipboard
            ${hub} pr show -c
            # copy PR link from clipboard to primary
            ${xclip} -selection clipboard -o | ${xclip} -selection primary -i -f
            '';
        };
      # select_wifi_network = {
      #   wpa_cli select_network $(wpa_cli list_networks | tail -n +3 | column -t -s'      ' | rofi -dmenu -p '> ' -no-custom -i | cut -f1 -d' ')
      # };
  };

  home.keyboard.layout = "gb";
  # home.{language,currency,time,etc.}- see `man home-configuration.nix`

  home.pointerCursor = {
    # TODO: but, but I just want to change the pointer size. Why do I have to
    # have this other stuff? Is there a default somewhere that I can override?
    size = 128;
    name = "Vanilla-DMZ";
    package = pkgs.vanilla-dmz;
    x11.enable = true;
  };

  xsession = {
    enable = true;
    windowManager.xmonad = {
      enable = true;
      enableContribAndExtras = true;
      config = ./xmonad.hs;
      extraPackages = haskellPackages: [ haskellPackages.lens ];
    };
  };

  services.kdeconnect = {
    enable = true;
    indicator = true;
  };

  services.picom = {
    # Notes
    # - fade not enabled because I found it to be annoying
    # - inactiveDim not enabled because I found it to be annoying
    # - vsync just works, which is quite nice
    # For a sample config see:
    # https://github.com/yshui/picom/blob/cd50596f0ed81c0aa28cefed62176bd6f050a1c6/picom.sample.conf
    package = pkgs.picom-next;
    enable = true;
    # TODO: vsync doesn't seem to be working
    vSync = true;
    settings = {
      # Sets the radius of rounded window corners. When > 0, the compositor will
      # round the corners of windows. Does not interact well with
      # `transparent-clipping`.
      corner-radius = 6;
      # Exclude conditions for rounded corners.
      rounded-corners-exclude = {
        # window_type = 'desktop' corresponds to e.g. window decorations applied by window
        # managers. I guess this matches anything that has _NET_WM_WINDOW_TYPE =
        # _NET_WM_WINDOW_TYPE_DESKTOP. Use xprop to find out where this applies.
        window_type = ["dock" "desktop"];
      };
    };
  };

  services.random-background = {
    enable = true;
    imageDirectory ="%h/Downloads/desktop-backgrounds/dump-wallpapercave.com";
    enableXinerama = true;
  };

  # https://wiki.archlinux.org/title/HiDPI
  services.grobi = {
    enable = true;
    executeAfter = [ "${config.xsession.windowManager.command} --restart" ];
    rules = [
      {
        name = "Mobile";
        outputs_connected = [ "eDP-1" ];
        configure_single = "eDP-1";
        primary = true;
        atomic = true;
        # From https://topics-cdn.dell.com/pdf/xps-15-9570-laptop_specifications_en-us.pdf
        # Width:  344.21 mm (13.55 in)
        # Height: 193.62 mm (7.62 in)
        # Diagonal inches: `calc 'sqrt(344.21 ^ 2 + 193.62 ^ 2) / 25.4'` = 15.55in
        # Resolution: 3840x2160
        # Diagonal pixels: `calc 'sqrt(3840 ^ 2 + 2160 ^ 2)'` = 4406px
        # Diagonal DPI: `calc '(sqrt(3840 ^ 2 + 2160 ^ 2)) / (sqrt(344.21 ^ 2 + 193.62 ^ 2) / 25.4)'` = 283.36
        # DPI should be 283
        # But that isn't very nice, in fact, so we set it to 192
        execute_after = [
          "${pkgs.xorg.xrandr}/bin/xrandr --dpi 192"
        ];
      }
      {
        name = "Van";
        outputs_connected = [ "DisplayPort-2" "DisplayPort-1" ];
        configure_row = [ "DisplayPort-2" "DisplayPort-1" ];
        primary = "DisplayPort-2";
        atomic = true;
      }
    ];
  };

  # TODO:
  # -[x] systemd service control (note: found sysz, considering this done)
  # -[x] mullvad exit nodes
  # -[x] pass integration
  # -[x] bluetooth device connection/configuration?
  # -[ ] wifi network selection?
  # -[ ] process killer
  # -[ ] rice rofi a bit more, perhaps more like http://thedarnedestthing.com/rofi%20columns
  # -[ ] modify the "open in terminal" command (or create a new one) to "open in terminal and hold open if appropriate"
  # -[ ] understand rofi-pass a bit better
  #      - How to generate a new password?
  #      - https://github.com/carnager/rofi-pass
  #      - https://github.com/carnager/rofi-pass/blob/master/config.example
  #      - replace rofi-pass with xmonad.prompt.pass
  programs.rofi = {
    pass = {
      enable = true;
      stores = [ "${config.home.homeDirectory}/.local/share/password-store" ];
      # TODO: doesn't seem to type "tab" correctly with autotype; test
      # TODO: decide whether I prefer (trust..) this or browserpass more.
      # TODO: consider https://hackage.haskell.org/package/xmonad-contrib-0.17.0/docs/XMonad-Prompt-Pass.html
      extraConfig = ''
        _rofi () {
            rofi -dmenu -i -no-auto-select "$@"
        }
        USERNAME_field='login'
        default-autotype 'login :tab pass'
        default_do='menu' # menu, autotype, copyPass, typeUser, typePass, copyUser, copyUrl, viewEntry, typeMenu, actionMenu, copyMenu, openUrl
        auto_enter='false'
        notify='false'
        '';
    };
    enable = true;
    theme = "sidebar";
    extraConfig = {
      # Remove existing c-j and c-k bindings:
      kb-remove-to-eol = ""; # default is c-k
      kb-accept-entry = "Return,KP_Enter"; # default contains c-j
      # .. so we can rebind c-j, c-k
      kb-row-up = "Up,Control+p,Control+k";
      kb-row-down = "Down,Control+n,Control+j,Shift+ISO_Left_Tab";
      kb-cancel = "Control+c";
      case-sensitive = false;
      font = "mono 19";
      terminal = "${pkgs.alacritty}/bin/alacritty";
      matching = "fuzzy";
    };
  };

  programs.broot = {
    # TODO: when exiting broot, restore the terminal cursor to the correct mode? (Can this be done
    # by zsh?)
    enable = true;
    enableZshIntegration = true;
    verbs = [
      {
        invocation  = "create {subpath}";
        execution   = "$EDITOR {directory}/{subpath}";
      }
      {
        invocation  = "edit";
        key         = "enter";
        # {line} is zero or 1 by default, which means that broot never opens vim where we
        # left off last time we opened the file. This command ought to resolve that problem.
        # It looks like broot quotes {file} so that it expands to e.g.
        #   nvim "bash cheatsheet"
        # instead of
        #   nvim bash cheatsheet
        # which would open the "bash" and the "cheatsheet" files.
        # This means that when we have double quotes in our external command string, they're
        # matched by the quotes inserted by broot.
        external    = "${pkgs.bash}/bin/bash -c \"[[ {line} -eq 0 ]] && ${pkgs.neovim}/bin/nvim '{file}' || ${pkgs.neovim}/bin/nvim '{file}' +{line}\"";
        leave_broot = false;
        apply_to    = "file";
      }
      {
        execution   = ":panel_left";
        key         = "ctrl-h";
      }
      {
        execution   = ":panel_right";
        key         = "ctrl-l";
      }
      { key = "ctrl-k"; internal = ":line_up"; }
      { key = "ctrl-j"; internal = ":line_down"; }
      { key = "ctrl-u"; internal = ":input_clear"; }
      { key = "ctrl-w"; internal = ":input_del_word_left"; }
      { key = "ctrl-h"; internal = ":toggle_hidden"; }
    ];
  };

  programs.password-store = {
    enable = true;
    package = pkgs.pass.withExtensions (exts: [exts.pass-otp]);
  };
  services.password-store-sync.enable = true;

  programs.git = {
    enable = true;
    userEmail = "mattkingston@protonmail.com";
    userName = "Matt Kingston";
    signing = {
      key = "0x29086A26F326ED5C";
      signByDefault = true;
    };
    aliases = {
      co   = "checkout";
      root = "rev-parse --show-toplevel";
      exec = "!exec "; # run commands in the git root dir, e.g. git exec cargo build or git exec nix build
    };
    ignores = [ ".envrc" ];
    extraConfig = {
      # Useful for extraConfig: https://git-scm.com/book/en/v2/Customizing-Git-Git-Configuration
      merge.tool                  = "vimdiff";
      mergetool.prompt            = "true";
      # TODO: get rid of one of $LOCAL $REMOTE $MERGED? Don't really want three-way split. Can we
      # just use vimdiff2? Or is it better to use opendiff, kdiff or something else for merges?
      "mergetool \"vimdiff\"".cmd = "nvim -d $LOCAL $REMOTE $MERGED -c '$wincmd w' -c 'wincmd J'";
      difftool.prompt             = "false";
      diff.tool                   = "nvimdiff";
      diff.algorithm              = "histogram";
      url                         = { "ssh://git@github.com" = { insteadOf = "https://github.com"; } ; } ;
      url                         = { "ssh://git@gitlab.modusbox.io" = { insteadOf = "https://gitlab.modusbox.io"; } ; } ;
      color.ui                    = "true";
      pull.rebase                 = "false";
      credential.helper           = "libsecret";
      push.autoSetupRemote        = "true";
    };
  };

  # Enable bash to get starship inside nix-shell
  programs.bash.enable = true;

  home.shellAliases =
    let
      bat = "${pkgs.bat}/bin/bat";
      calc = "${pkgs.calc}/bin/calc";
      date = "${pkgs.coreutils}/bin/date";
      expr = "${pkgs.coreutils}/bin/expr";
      rg = "${pkgs.ripgrep}/bin/rg";
      sk = "${pkgs.skim}/bin/sk";
      nvim = "${pkgs.neovim}/bin/nvim";
      kubectl = "${pkgs.kubectl}/bin/kubectl";
      systemctl = "${pkgs.systemd}/bin/systemctl";
      git = "${pkgs.git}/bin/git";
      find = "${pkgs.findutils}/bin/find";
      exa = "${pkgs.exa}/bin/exa";
      xclip = "${pkgs.xclip}/bin/xclip";
    in {
      # TODO: some aliases to use the fuzzy finder for searching/killing processes. Related: is
      # there some TUI utility out there that shows the process tree and allows process killing,
      # exploration etc.? Rofi?
      # TODO: note that some of these utilities have man pages, but when they're wrapped like
      # this, the man is not installed. buku is one example of such. How to work around this?
      # Perhaps wrapping them?
      b64 = "${pkgs.coreutils}/bin/base64";
      b64d = "${pkgs.coreutils}/bin/base64 --decode";
      buku = "${pkgs.buku}/bin/buku --db ${config.home.homeDirectory}/.dotfiles/bookmarks.db";
      chown = "chown -h";
      chmox = "${pkgs.coreutils}/bin/chmod +x";
      chmow = "${pkgs.coreutils}/bin/chmod +w";
      df = "${pkgs.lfs}/bin/lfs -c +inodes_use_percent";
      gacm = "${git} add -u; ${git} commit -m";
      gau = "${git} add -u";
      gbl = "${git} branch -liar";
      gcm = "${git} commit -m";
      gcob = "${git} checkout -b";
      gco = "${git} checkout";
      gcw = "${git} commit -m \"whatever\"";
      gdt = "${git} difftool";
      glns = "${git} log --name-status";
      gpl = "${git} pull";
      gp = "${git} push";
      gpo = "${git} push -u origin";
      gr = "cd $(${git} rev-parse --show-toplevel)";
      grohm = "${git} stash push -m \"reset $(${date} -u -Iseconds)\" && ${git} reset --hard origin/master";
      gst = "${git} status";
      gsti = "${git} status --ignored";
      gsw = "${git} switch";
      findfontfile = "${pkgs.fontconfig}/bin/fc-list | ${sk} | ${pkgs.coreutils}/bin/cut -d: -f1";
      kcd = "${kubectl} delete";
      kcds = "${kubectl} describe";
      kce = "${kubectl} edit";
      kcgj = "${kubectl} get -o json";
      kcg = "${kubectl} get";
      kc = "${kubectl}";
      kclf = "${kubectl} logs -f";
      kcl = "${kubectl} logs";
      kclt = "${kubectl} logs -f --tail=0";
      kcpf = "${kubectl} port-forward";
      kcp = "${kubectl} patch";
      kcx = "${kubectl} exec";
      kz = "${pkgs.kustomize}/bin/kustomize";
      lg = "${pkgs.lazygit}/bin/lazygit";
      ls = "${exa} --all --long --git --time-style long-iso";
      mkcdt = "cd $(${config.home.homeDirectory}/${config.home.file.mktempdir.target})";
      refcp = "${git} rev-parse HEAD | tr -d '\n' | ${xclip} -i -sel clipboard -f | ${xclip} -i -sel primary -f";
      rm = "${pkgs.trash-cli}/bin/trash-put";
      sc = "${systemctl}";
      scf = "${systemctl} --state=failed";
      scratch = "cd ~/projects/scratch";
      scur = "${systemctl} --user restart";
      scus = "${systemctl} --user status";
      scu = "${systemctl} --user";
      ssh = "${pkgs.mosh}/bin/mosh --predict=experimental";
      stripcolours="${pkgs.gnused}/bin/sed -r 's/\\x1B\\[([0-9]{1,2}(;[0-9]{1,2})?)?[mGK]//g'";
      tree = "${exa} --all -T --git-ignore -I.git";
      ts = ''
        ${sk} \
          --delimiter ':' \
          --ansi \
          -i \
          -c '${rg} -n --ignore-vcs --color=always "{}"' \
          --preview '${bat} --style=numbers,changes --color=always -r "$(${calc} -p "floor(max(1, $(${expr} {2}) - $LINES / 2))"):$(${calc} -p "floor($LINES + max(0, $(${expr} {2}) - $LINES / 2))")" -H{2} {1}'
        '';
      v = "${nvim}";
      vd = "${nvim} -d";
      # TODO: the following, but with a language server generating the input list i.e. tokens?
      # Perhaps look at https://github.com/lotabout/skim.vim
      # TODO: would be nice to add a search term to nvim startup, e.g. `nvim {1} +{2} +/{0}`. At
      # the time of writing, skim doesn't supply the current search term to the executed program,
      # AFAICT. (Or perhaps we could jump to vim line+column?)
      # TODO: making this a shell function would let us take an optional argument to the --query
      # parameter, so we could use `vs "some text to search"`. The advantage of this would be
      # that this query would go into the shell command history.
      # TODO: can this be replaced with Broot and c/ (content search) functionality?
      vs = ''
        ${sk} \
          --bind "enter:execute(${nvim} {1} +{2})" \
          --delimiter ':' \
          --ansi \
          -i \
          -c '${rg} -n --ignore-vcs --hidden --smart-case --color=always "{}"' \
          --preview '${bat} --style=numbers,changes --color=always -r "$(${calc} -p "floor(max(1, $(${expr} {2}) - $LINES / 2))"):$(${calc} -p "floor($LINES + max(0, $(${expr} {2}) - $LINES / 2))")" -H{2} {1}'
      '';
      watch = "${pkgs.viddy}/bin/viddy";
      weather = "${pkgs.curl}/bin/curl http://v2.wttr.in";
    };

  programs.zsh = {
    # TODO: migrating zshrc to here means it's possible to enforce dependencies. For example,
    # instead of aliasing 'kc' to 'kubectl', it's possible to alias 'kc' to
    # ${pkgs.kubectl}/bin/kubectl. However, this would mean reducing portability.
    enable = true;
    enableAutosuggestions = true;
    enableCompletion = true;
    # environment.pathsToLink = [ "/share/zsh" ];
    initExtra = builtins.readFile ./.zshrc;
    plugins = customZshPlugins;
  };

  programs.zsh.shellGlobalAliases = {
      pg = "| grep";
      dots = "${config.home.homeDirectory}/.dotfiles";
  };

  programs.neovim = {
    enable = true;
    viAlias = true;
    vimAlias = true;
    # TODO: consider different colorschemes for different file types with autocommands?
    # TODO: move config out of .vim/after/plugins (or not? no harm in it being in different files
    # related to each plugin; and probably a little bit more portable outside of a nix or
    # nix-like system)
    extraConfig =
      (builtins.readFile ./init.vim) + "\n" +
      (filesIn ./.vim/after/plugin "vim");
    # package = pkgs.neovim-nightly;
    plugins = with pkgs.vimPlugins; with customVimPlugins; [
      # list vim packages:
      # > nix-env -f '<nixpkgs>' -qaP -A vimPlugins
      ale
      awesome-vim-colorschemes
      cmp-nvim-lsp
      cmp_luasnip
      dart-vim-plugin
      easy-align
      editorconfig-vim
      haskell-vim
      hop-nvim
      lualine-nvim
      luasnip
      markdown-preview-nvim
      nvim-autopairs
      nvim-cmp
      nvim-lspconfig
      nvim-treesitter
      nvim-treesitter-context
      # TODO: nvim-treesitter-textobjects
      #       - use this to have comment textobjects using @comment.outer (see the treesitter textobjects docs)?
      # TODO: nvim-treesitter-refactor
      repeat
      rust-vim
      sensible
      sideways-vim
      solarized
      surround
      # TODO: https://github.com/nvim-telescope/telescope.nvim
      tcomment_vim
      # TODO: vim-textobj-comment # doesn't have 'vspec' file for modern vim plugins? Or does it need textobj-user?
      typescript-vim
      ultisnips
      vim-autoformat
      vim-capnp
      vim-gh-line
      vim-indent-object
      vim-javascript
      vim-markdown
      vim-nix
      vim-toml
      vim-yaml-folds
      zig-vim
    ];
  };

  home.sessionVariables = {
    EDITOR  = "${pkgs.neovim}/bin/nvim";
    BROWSER = "${config.home.homeDirectory}/${config.home.file.select-browser.target}";
    TERMCMD = "${pkgs.alacritty}/bin/alacritty";
    TEMPDIR = "$HOME/${userTempDirName}/";
  };

  systemd.user.tmpfiles.rules = [
    "d ${config.home.homeDirectory}/${userTempDirName} 1777 ${config.home.username} users 7d"
  ];

  home.sessionPath = [
    "$HOME/${userScriptDir}"
  ];

  # systemd.user.services.notification-center = {
  #   # Derived from https://github.com/phuhl/linux_notification_center/blob/d31867472c35a09562c832b0a589479930c52b86/deadd-notification-center.service.in
  #   Unit = {
  #     Description = "Deadd Notification Center";
  #   };
  #
  #   Install.WantedBy = [ "multi-user.target" ];
  #
  #   Service = {
  #     ExecStart   = "${pkgs.deadd-notification-center}/bin/deadd-notification-center";
  #     KillSignal  = "SIGTERM";
  #     Restart     = "always";
  #     RestartSec  = 10;
  #     PIDFile     = "/run/notification-center.pid";
  #     # TODO: don't hard-code these?
  #     Environment = [ "DISPLAY=:0" "DBUS_SESSION_BUS_ADDRESS=unix:path=/run/user/1000/bus" "NO_AT_BRIDGE=1" ];
  #   };
  # };

  # TODO: auto-restart services on system update?
  systemd.user.startServices = "suggest";
  systemd.user.services.chromium = basicService {
    desc = "Chromium";
    cmd = "${pkgs.chromium}/bin/chromium";
  };
  systemd.user.services.firefox = basicService {
    desc = "Firefox";
    # TODO: extract all bash scripts into their own derivations so that they're accessible from
    # systemd services, then make this service use the select-firefox-profile script
    # TODO: try firefox --no-remote -P
    #       failed at the time of writing but not sure why
    cmd = "${pkgs.firefox}/bin/firefox";
    env = "MOZ_USE_XINPUT2=1"; # https://wiki.archlinux.org/title/Firefox#Touchscreen_gestures_and_pixel-perfect_trackpad_scrolling
  };
  systemd.user.services.zeal = basicService {
    desc = "Zeal";
    cmd = "${pkgs.zeal}/bin/zeal";
  };
  systemd.user.services.keybase-gui = basicService {
    desc = "Keybase GUI";
    cmd = "${pkgs.keybase-gui}/bin/keybase-gui";
    env = "NIX_SKIP_KEYBASE_CHECKS=1"; # TODO: Should probably investigate whether this is still necessary
  };
  systemd.user.timers.empty-trash = {
    Unit = {
      Description = "Empty trash daily";
    };
    Timer = {
      OnCalendar = "weekly";
      Persistent = true;
    };
    Install = {
      WantedBy = [ "timers.target" ];
    };
  };
  systemd.user.services.empty-trash = {
    Unit = {
      Description = "Empty trash older than 30 days";
    };

    Service = {
      ExecStart = "${pkgs.trash-cli}/bin/trash-empty 30";
      KillSignal = "SIGTERM";
      TimeoutStopSec = 600;
    };
  };
  systemd.user.services.mullvad = {
    Unit = {
      Description = "Mullvad";
      After = [ "network-pre.target" ];
      PartOf = [ "network-online.target" ];
    };

    Install = {
      WantedBy = [ "network-online.target" ];
    };

    Service = {
      ExecStart = "${pkgs.mullvad-vpn}/bin/mullvad connect --wait";
      KillSignal = "SIGTERM";
      TimeoutStopSec = 60;
    };
  };
  systemd.user.services.protonmail = firefoxService
    { name = "protonmail"; desc = "ProtonMail"; url = "mail.protonmail.com"; };
  systemd.user.services.calendar = firefoxService
    { name = "calendar"; desc = "iCloud Calendar"; url = "icloud.com/calendar/"; };
  systemd.user.services.contacts = firefoxService
    { name = "contacts"; desc = "iCloud Contacts"; url = "icloud.com/contacts/"; };
  systemd.user.services.whatsapp = firefoxService
    { name = "whatsapp"; desc = "WhatsApp Web"; url = "web.whatsapp.com"; };
  # TODO: work-gmail, work-calendar? Or am I just going to need to be logged in to the work
  # Google Workspace in my normal browsing session anyway? Should I have work gmail + calendar in
  # their own workspace in any case? Should there be a separate browser for stuff that needs to be
  # logged in to G workspace? Probably that's the way to go actually.
  systemd.user.services.slack = firefoxService
    { name = "slack"; desc = "Slack"; url = work.slack-url; };
  systemd.user.services.gmail = firefoxService
    { name = "gmail"; desc = "Gmail"; url = "mail.google.com"; };
  systemd.user.services.fbmessenger = firefoxService
    { name = "messenger"; desc = "Facebook Messenger"; url = "messenger.com"; };
  systemd.user.services.signal = constrainedService
    { desc = "Signal"; cmd = "${pkgs.signal-desktop}/bin/signal-desktop"; };
  systemd.user.services.spotify = constrainedService
    { desc = "Spotify"; cmd = "${pkgs.spotifywm}/bin/spotifywm"; };
  # From: https://nixos.wiki/wiki/Bluetooth#Using_Bluetooth_headset_buttons_to_control_media_player
  systemd.user.services.mpris-proxy = {
    Unit.Description = "Mpris proxy";
    Unit.After = [ "network.target" "sound.target" ];
    Service.ExecStart = "${pkgs.bluez}/bin/mpris-proxy";
    Install.WantedBy = [ "default.target" ];
  };
  systemd.user.services.pueued = {
    # adapted from https://github.com/Nukesor/pueue/blob/ce86efd61f052bf144a1da972512455700057681/utils/pueued.service
    # note that it's possible this is out of date
    Unit.Description = "Pueue Daemon - CLI process scheduler and manager";
    Service = {
      Restart="no";
      ExecStart="${pkgs.pueue}/bin/pueued";
      ExecReload="${pkgs.pueue}/bin/pueued";
    };
    Install.WantedBy=[ "default.target" ];
  };

  services.keynav.enable = true;

  home.packages = with pkgs; [
    alacritty
    android-file-transfer
    authy
    bat
    cabal2nix
    calc
    cargo
    cargo-edit
    crow-translate # there is also translate-shell as an alternative
    dnsutils
    doctl
    entr
    # epick
    exa
    fd
    ffmpeg
    freetube
    gh
    git
    git-crypt
    gitAndTools.hub
    gnumake
    gnumeric
    gnupg
    gron
    haskell-language-server
    inetutils
    ijq
    jdt-language-server
    jid
    jq
    keybase-gui
    kubeconform
    kubectl
    kustomize
    ldns # drill
    libnotify
    libsecret # needed for git.extraConfig.credential.helper
    lnav
    moreutils
    mosh
    mpv
    mullvad-vpn
    myDsq
    myNode
    mycli
    mysql
    ncpamixer
    # TODO: nix-du
    nix-prefetch-git
    nodePackages.typescript-language-server
    openssh
    openssl
    pciutils
    podman-compose
    pueue
    python310Packages.sqlparse # TODO: is this for linting/editing SQL in vim? Remove?
    pwgen
    ripgrep
    rnix-lsp
    rust-analyzer
    rustc
    signal-desktop
    skaffold
    skim
    socat
    spotify-tui
    sysz
    # TODO: time tracker
    # programs.timewarrior (do others exist in home-configuration.nix?)
    # timewarrior
    # timetrap
    trash-cli
    tree
    tree-sitter
    tuc
    unzip
    usbutils
    viddy
    vlc
    wezterm
    wireguard-tools
    xclip
    xh
    xorg.xdpyinfo
    xsel
    xxd
    yaml-language-server
    yamllint
    yarn
    yj
    yq
    yt-dlp
    zeal
    zig # zig works as a C compiler for the nvim treesitter implementation to compile parsers, so required here
    zip
    zoom-us
  ];

  fonts.fontconfig.enable = true;

  # services.mpd.enable = true;
  services.flameshot.enable = true;
  services.unclutter.enable = true;
  services.keybase.enable = true;
  services.kbfs.enable = true;
  services.dunst = {
    enable = true;
    settings = {
      global = {
        follow = "keyboard";
        history_length = "100";
      };
    };
  };

  services.redshift = {
    enable = true;
    # Paris
    latitude = "48.8566";
    longitude = "2.3522";
  };

  # TODO: manage keys with home manager? See programs.gpg in `man home-configuration.nix`
  # TODO: unlock ssh key using same password as gpg key
  # TODO: unlock on login; could use programs.zsh.loginExtra or perhaps on xmonad startup?
  # TODO: read man home-configuration on gpg-agent
  # TODO: https://www.linode.com/docs/guides/gpg-key-for-ssh-authentication/
  # TODO: https://rzetterberg.github.io/yubikey-gpg-nixos.html
  # TODO: pinentryFlavor = "curses";
  programs.gpg.enable = true;
  services.gpg-agent = {
    enable = true;
    enableSshSupport = true;
    defaultCacheTtl = 60 * 60 * 4; # four hours
    defaultCacheTtlSsh = 60 * 60 * 4; # four hours
    sshKeys = [ "4B72C49125846589E48EC27E1B834B035EAF81E1" ];
  };

  # TODO: turn the screen off immediately after we lock it. (Or just suspend?).
  # TODO: unlock keyring after unlock? In the short term, perhaps just pop up the unlock dialog
  # when the screen unlocks. But should be able to use pam to actually unlock.
  services.screen-locker = {
    enable = true;
    inactiveInterval = 5;
    # todo ; turn off screen
    lockCmd = config.home.homeDirectory + "/" + config.home.file.invalidategpgcacheonscreenlock.target;
  };

  services.poweralertd.enable = true;

  xdg = let
    broot            = "broot";
    browser-selector = "browser-selector";
    ephemeral-vim    = "ephemeral-vim";
    feh              = "feh";
  in {
    enable = true;
    mime.enable = true;
    # TODO:
    # - should I fork after these? Or should I leave that up to the calling application?
    # - mailto:
    # - sgnl:
    # - make sure the browser opens "open directory" in broot
    # - when some file is opened from Firefox, from the downloads window, we get a strange thing in
    #   vim where it's starting in some odd environment without e.g. git available to it
    # - could be nice to have some icons for the desktop entries, because it'll then be more
    #   obvious where something will open from e.g. Firefox
    # Debugging this:
    # - Make a fake file, e.g. rubbish.csv
    # - Use `xdg-mime query filetype rubbish.csv` to check the mime type
    # - Add a desktop entry to this configuration to support that mime type
    # - Potentially set the default application for that mime type
    # - Use `xdg-mime query default text/csv` to check the default for e.g. `text/csv`
    # https://wiki.archlinux.org/title/Desktop_entries
    #
    # From `man xdg-desktop-menu`:
    #
    #   A list of categories separated by semi-colons. A category is a keyword that describes
    #   and classifies the application. By default applications are organized in the
    #   application menu based on category. When menu entries are explicitly assigned to a new
    #   submenu it is not necessary to list any categories.
    #
    #   When using categories it is recommended to include one of the following categories:
    #   AudioVideo, Development, Education, Game, Graphics, Network, Office, Settings, System,
    #   Utility.
    #
    #   See Appendix A of the XDG Desktop Menu Specification for information about additional
    #   categories:
    #   http://standards.freedesktop.org/menu-spec/menu-spec-1.0.html#category-registry
    desktopEntries = rec {
      translate-x11-primary-selection = {
        name        = "Translate primary X11 selection";
        genericName = "Selection translator";
        exec        = "${config.home.homeDirectory}/${config.home.file.hold.target} ${config.home.homeDirectory}/${config.home.file.translate-x11-primary-selection.target}";
        terminal    = true; # haven't figured out how to have crow open with some text preloaded
        categories  = [ "Languages" "TextTools" ];
      };
      type-clipboard = {
        name        = "Type clipboard";
        genericName = "Type the contents of the clipboard";
        exec        = "${config.home.homeDirectory}/${config.home.file.type-clipboard.target}";
        terminal    = false;
        categories  = [ "Utility" "TextTools" ];
      };
      "${browser-selector}" = {
        name        = "Browser selector";
        genericName = "Web Browser";
        exec        = "${config.home.homeDirectory}/${config.home.file.select-browser.target} %U";
        terminal    = false;
        categories  = [ "Network" "WebBrowser" ];
        mimeType    = [
          "x-scheme-handler/about"
          "x-scheme-handler/unknown"
          "x-scheme-handler/http"
          "x-scheme-handler/https"
          "image/svg+xml"
        ];
      };
      # TODO: desktop entry for ncpamixer?
      bt-conn = {
        name        = "bt-conn";
        genericName = "Rofi bluetooth connection menu";
        exec        = "${config.home.homeDirectory}/${config.home.file.bt-conn.target}";
        terminal    = false;
        categories  = [ "System" "Settings" ];
      };
      edot = {
        name        = "edot";
        genericName = "Edit dotfiles";
        exec        = "${config.home.homeDirectory}/${config.home.file.edot.target}";
        terminal    = true;
        categories  = [ "System" "Settings" ];
      };
      # epick = {
      #   name        = "epick";
      #   genericName = "Color picker";
      #   exec        = "${pkgs.epick}/bin/epick";
      #   terminal    = false;
      #   categories  = [ "Graphics" ];
      # };
      hibernate = {
        name        = "Hibernate";
        genericName = "Hibernate system";
        exec        = "${pkgs.systemd}/bin/systemctl hibernate";
        terminal    = true;
        categories  = [ "System" "Utility" ];
      };
      notes = {
        name        = "notes";
        genericName = "Edit general notes";
        exec        = "${config.home.homeDirectory}/${config.home.file.notes.target}";
        terminal    = true;
        categories  = [ "Utility" "TextTools" ];
      };
      poweroff = {
        name        = "Poweroff";
        genericName = "Shutdown system (poweroff)";
        exec        = "${pkgs.systemd}/bin/systemctl poweroff";
        terminal    = true;
        categories  = [ "System" "Utility" ];
      };
      reboot = {
        name        = "Reboot";
        genericName = "Reboot system";
        exec        = "${pkgs.systemd}/bin/systemctl reboot";
        terminal    = true;
        categories  = [ "System" "Utility" ];
      };
      sleep = {
        name        = "Suspend";
        genericName = "Suspend system to RAM (sleep)";
        exec        = "${pkgs.systemd}/bin/systemctl suspend";
        terminal    = true;
        categories  = [ "System" "Utility" ];
      };
      tv = {
        name        = "tv";
        genericName = "Edit development notes";
        exec        = "${config.home.homeDirectory}/${config.home.file.tv.target}";
        terminal    = true;
        categories  = [ "Utility" "TextTools" "Development" ];
      };
      sysz = {
        name        = "sysz";
        genericName = "Systemd TUI";
        exec        = "${pkgs.sysz}/bin/sysz";
        terminal    = true;
        categories  = [ "System" "Settings" ];
      };
      ephemeral-vim = {
        name        = "Ephemeral vim";
        genericName = "Text Editor";
        exec        = "${pkgs.alacritty}/bin/alacritty -e ${pkgs.neovim}/bin/nvim %U";
        terminal    = false;
        categories  = [ "Development" ];
        mimeType    = [
          "application/javascript"
          "application/json"
          # TODO: probably something better than vim to open archive files with.
          # One idea: https://github.com/Canop/broot/issues/197
          "application/x-bzip-compressed-tar"
          "application/x-compressed-tar"
          "application/x-shellscript"
          "application/zip"
          "text/english"
          "text/html"
          "text/plain"
          "text/rust"
          "text/xml"
          # TODO:
          # "text/x-c"
          # "text/x-c++"
          # "text/x-c++hdr"
          # "text/x-chdr"
          # "text/x-c++src"
          # "text/x-csrc"
          # "text/x-java"
          # "text/x-makefile"
          # "text/x-moc"
          # "text/x-pascal"
          # "text/x-tcl"
          # "text/x-tex"
        ];
      };
      vim = {
        name        = "Vim";
        genericName = "Text Editor";
        exec        = "${pkgs.neovim}/bin/nvim %U";
        terminal    = true;
        categories  = [ "Development" ];
        mimeType    = ephemeral-vim.mimeType;
      };
      "${feh}" = {
        name        = "Feh";
        genericName = "Image Viewer";
        exec        = "${pkgs.feh}/bin/feh -Z %U";
        terminal    = false;
        categories  = [ "Graphics" "Viewer" ];
        mimeType    = [
          "image/jpeg"
          "image/bmp"
          "image/png"
          "image/tiff"
          "image/x-icon"
          "image/x-xpixmap"
          "image/x-xbitmap"
        ];
      };
      "${broot}" = {
        name        = "Broot";
        genericName = "File Browser";
        exec        = "${pkgs.broot}/bin/broot %U";
        terminal    = true;
        categories  = [ "Utility" "FileTools" "FileManager" ];
        mimeType    = [ "inode/directory" ];
      };
    };
    # TODO: somehow chromium overrides these *sigh*. Where is its desktop file?
    mimeApps.defaultApplications = {
      "inode/directory"                   = "${broot}.desktop";
      "text/html"                         = "${browser-selector}.desktop";
      "x-scheme-handler/http"             = "${browser-selector}.desktop";
      "x-scheme-handler/https"            = "${browser-selector}.desktop";
      "x-scheme-handler/about"            = "${browser-selector}.desktop";
      "x-scheme-handler/unknown"          = "${browser-selector}.desktop";
      "image/jpeg"                        = "${feh}.desktop";
      "image/bmp"                         = "${feh}.desktop";
      "image/png"                         = "${feh}.desktop";
      "image/tiff"                        = "${feh}.desktop";
      "image/x-icon"                      = "${feh}.desktop";
      "image/x-xpixmap"                   = "${feh}.desktop";
      "image/x-xbitmap"                   = "${feh}.desktop";
      "application/javascript"            = "${ephemeral-vim}.desktop";
      "application/json"                  = "${ephemeral-vim}.desktop";
      "application/x-bzip-compressed-tar" = "${ephemeral-vim}.desktop";
      "application/x-compressed-tar"      = "${ephemeral-vim}.desktop";
      "application/x-shellscript"         = "${ephemeral-vim}.desktop";
      "application/zip"                   = "${ephemeral-vim}.desktop";
      "text/english"                      = "${ephemeral-vim}.desktop";
      "text/plain"                        = "${ephemeral-vim}.desktop";
      "text/rust"                         = "${ephemeral-vim}.desktop";
      # TODO:
      # "application/x-bittorrent"          = "${torrent}.desktop";
      # "x-scheme-handler/magnet"           = "${torrent}.desktop";
    };
    configFile = {
      # Tree sitter grammars. It might actually be that if we list these in init.vim the tree
      # sitter plugin will install these for us.
      "nvim/parser/agda.so".source              = "${pkgs.tree-sitter-grammars.tree-sitter-agda}/parser";
      "nvim/parser/bash.so".source              = "${pkgs.tree-sitter-grammars.tree-sitter-bash}/parser";
      "nvim/parser/beancount.so".source         = "${pkgs.tree-sitter-grammars.tree-sitter-beancount}/parser";
      "nvim/parser/bibtex.so".source            = "${pkgs.tree-sitter-grammars.tree-sitter-bibtex}/parser";
      "nvim/parser/c.so".source                 = "${pkgs.tree-sitter-grammars.tree-sitter-c}/parser";
      "nvim/parser/c-sharp.so".source           = "${pkgs.tree-sitter-grammars.tree-sitter-c-sharp}/parser";
      "nvim/parser/clojure.so".source           = "${pkgs.tree-sitter-grammars.tree-sitter-clojure}/parser";
      "nvim/parser/cmake.so".source             = "${pkgs.tree-sitter-grammars.tree-sitter-cmake}/parser";
      "nvim/parser/comment.so".source           = "${pkgs.tree-sitter-grammars.tree-sitter-comment}/parser";
      "nvim/parser/commonlisp.so".source        = "${pkgs.tree-sitter-grammars.tree-sitter-commonlisp}/parser";
      "nvim/parser/cpp.so".source               = "${pkgs.tree-sitter-grammars.tree-sitter-cpp}/parser";
      "nvim/parser/css.so".source               = "${pkgs.tree-sitter-grammars.tree-sitter-css}/parser";
      "nvim/parser/cuda.so".source              = "${pkgs.tree-sitter-grammars.tree-sitter-cuda}/parser";
      "nvim/parser/dart.so".source              = "${pkgs.tree-sitter-grammars.tree-sitter-dart}/parser";
      "nvim/parser/devicetree.so".source        = "${pkgs.tree-sitter-grammars.tree-sitter-devicetree}/parser";
      "nvim/parser/dockerfile.so".source        = "${pkgs.tree-sitter-grammars.tree-sitter-dockerfile}/parser";
      "nvim/parser/dot.so".source               = "${pkgs.tree-sitter-grammars.tree-sitter-dot}/parser";
      "nvim/parser/elisp.so".source             = "${pkgs.tree-sitter-grammars.tree-sitter-elisp}/parser";
      "nvim/parser/elixir.so".source            = "${pkgs.tree-sitter-grammars.tree-sitter-elixir}/parser";
      "nvim/parser/elm.so".source               = "${pkgs.tree-sitter-grammars.tree-sitter-elm}/parser";
      "nvim/parser/embedded-template.so".source = "${pkgs.tree-sitter-grammars.tree-sitter-embedded-template}/parser";
      "nvim/parser/erlang.so".source            = "${pkgs.tree-sitter-grammars.tree-sitter-erlang}/parser";
      "nvim/parser/fennel.so".source            = "${pkgs.tree-sitter-grammars.tree-sitter-fennel}/parser";
      "nvim/parser/fish.so".source              = "${pkgs.tree-sitter-grammars.tree-sitter-fish}/parser";
      "nvim/parser/fluent.so".source            = "${pkgs.tree-sitter-grammars.tree-sitter-fluent}/parser";
      "nvim/parser/fortran.so".source           = "${pkgs.tree-sitter-grammars.tree-sitter-fortran}/parser";
      "nvim/parser/gdscript.so".source          = "${pkgs.tree-sitter-grammars.tree-sitter-gdscript}/parser";
      "nvim/parser/glimmer.so".source           = "${pkgs.tree-sitter-grammars.tree-sitter-glimmer}/parser";
      "nvim/parser/glsl.so".source              = "${pkgs.tree-sitter-grammars.tree-sitter-glsl}/parser";
      "nvim/parser/go.so".source                = "${pkgs.tree-sitter-grammars.tree-sitter-go}/parser";
      "nvim/parser/godot-resource.so".source    = "${pkgs.tree-sitter-grammars.tree-sitter-godot-resource}/parser";
      "nvim/parser/gomod.so".source             = "${pkgs.tree-sitter-grammars.tree-sitter-gomod}/parser";
      "nvim/parser/gowork.so".source            = "${pkgs.tree-sitter-grammars.tree-sitter-gowork}/parser";
      "nvim/parser/graphql.so".source           = "${pkgs.tree-sitter-grammars.tree-sitter-graphql}/parser";
      "nvim/parser/haskell.so".source           = "${pkgs.tree-sitter-grammars.tree-sitter-haskell}/parser";
      "nvim/parser/hcl.so".source               = "${pkgs.tree-sitter-grammars.tree-sitter-hcl}/parser";
      "nvim/parser/heex.so".source              = "${pkgs.tree-sitter-grammars.tree-sitter-heex}/parser";
      "nvim/parser/hjson.so".source             = "${pkgs.tree-sitter-grammars.tree-sitter-hjson}/parser";
      "nvim/parser/html.so".source              = "${pkgs.tree-sitter-grammars.tree-sitter-html}/parser";
      "nvim/parser/http.so".source              = "${pkgs.tree-sitter-grammars.tree-sitter-http}/parser";
      "nvim/parser/janet-simple.so".source      = "${pkgs.tree-sitter-grammars.tree-sitter-janet-simple}/parser";
      "nvim/parser/java.so".source              = "${pkgs.tree-sitter-grammars.tree-sitter-java}/parser";
      "nvim/parser/javascript.so".source        = "${pkgs.tree-sitter-grammars.tree-sitter-javascript}/parser";
      "nvim/parser/jsdoc.so".source             = "${pkgs.tree-sitter-grammars.tree-sitter-jsdoc}/parser";
      "nvim/parser/json.so".source              = "${pkgs.tree-sitter-grammars.tree-sitter-json}/parser";
      "nvim/parser/json5.so".source             = "${pkgs.tree-sitter-grammars.tree-sitter-json5}/parser";
      "nvim/parser/julia.so".source             = "${pkgs.tree-sitter-grammars.tree-sitter-julia}/parser";
      "nvim/parser/kotlin.so".source            = "${pkgs.tree-sitter-grammars.tree-sitter-kotlin}/parser";
      "nvim/parser/latex.so".source             = "${pkgs.tree-sitter-grammars.tree-sitter-latex}/parser";
      "nvim/parser/ledger.so".source            = "${pkgs.tree-sitter-grammars.tree-sitter-ledger}/parser";
      "nvim/parser/llvm.so".source              = "${pkgs.tree-sitter-grammars.tree-sitter-llvm}/parser";
      "nvim/parser/lua.so".source               = "${pkgs.tree-sitter-grammars.tree-sitter-lua}/parser";
      "nvim/parser/make.so".source              = "${pkgs.tree-sitter-grammars.tree-sitter-make}/parser";
      "nvim/parser/markdown.so".source          = "${pkgs.tree-sitter-grammars.tree-sitter-markdown}/parser";
      "nvim/parser/markdown_inline.so".source   = "${pkgs.tree-sitter-grammars.tree-sitter-markdown-inline}/parser";
      "nvim/parser/nix.so".source               = "${pkgs.tree-sitter-grammars.tree-sitter-nix}/parser";
      "nvim/parser/norg.so".source              = "${pkgs.tree-sitter-grammars.tree-sitter-norg}/parser";
      "nvim/parser/ocaml.so".source             = "${pkgs.tree-sitter-grammars.tree-sitter-ocaml}/parser";
      "nvim/parser/ocaml_interface.so".source   = "${pkgs.tree-sitter-grammars.tree-sitter-ocaml-interface}/parser";
      "nvim/parser/org-nvim.so".source          = "${pkgs.tree-sitter-grammars.tree-sitter-org-nvim}/parser";
      "nvim/parser/perl.so".source              = "${pkgs.tree-sitter-grammars.tree-sitter-perl}/parser";
      "nvim/parser/pgn.so".source               = "${pkgs.tree-sitter-grammars.tree-sitter-pgn}/parser";
      "nvim/parser/php.so".source               = "${pkgs.tree-sitter-grammars.tree-sitter-php}/parser";
      "nvim/parser/pioasm.so".source            = "${pkgs.tree-sitter-grammars.tree-sitter-pioasm}/parser";
      "nvim/parser/prisma.so".source            = "${pkgs.tree-sitter-grammars.tree-sitter-prisma}/parser";
      "nvim/parser/pug.so".source               = "${pkgs.tree-sitter-grammars.tree-sitter-pug}/parser";
      "nvim/parser/python.so".source            = "${pkgs.tree-sitter-grammars.tree-sitter-python}/parser";
      "nvim/parser/ql.so".source                = "${pkgs.tree-sitter-grammars.tree-sitter-ql}/parser";
      "nvim/parser/query.so".source             = "${pkgs.tree-sitter-grammars.tree-sitter-query}/parser";
      "nvim/parser/r.so".source                 = "${pkgs.tree-sitter-grammars.tree-sitter-r}/parser";
      "nvim/parser/regex.so".source             = "${pkgs.tree-sitter-grammars.tree-sitter-regex}/parser";
      "nvim/parser/rego.so".source              = "${pkgs.tree-sitter-grammars.tree-sitter-rego}/parser";
      "nvim/parser/rst.so".source               = "${pkgs.tree-sitter-grammars.tree-sitter-rst}/parser";
      "nvim/parser/ruby.so".source              = "${pkgs.tree-sitter-grammars.tree-sitter-ruby}/parser";
      "nvim/parser/rust.so".source              = "${pkgs.tree-sitter-grammars.tree-sitter-rust}/parser";
      "nvim/parser/scala.so".source             = "${pkgs.tree-sitter-grammars.tree-sitter-scala}/parser";
      "nvim/parser/scheme.so".source            = "${pkgs.tree-sitter-grammars.tree-sitter-scheme}/parser";
      "nvim/parser/scss.so".source              = "${pkgs.tree-sitter-grammars.tree-sitter-scss}/parser";
      "nvim/parser/sparql.so".source            = "${pkgs.tree-sitter-grammars.tree-sitter-sparql}/parser";
      "nvim/parser/sql.so".source               = "${pkgs.tree-sitter-grammars.tree-sitter-sql}/parser";
      "nvim/parser/supercollider.so".source     = "${pkgs.tree-sitter-grammars.tree-sitter-supercollider}/parser";
      "nvim/parser/surface.so".source           = "${pkgs.tree-sitter-grammars.tree-sitter-surface}/parser";
      "nvim/parser/svelte.so".source            = "${pkgs.tree-sitter-grammars.tree-sitter-svelte}/parser";
      "nvim/parser/tiger.so".source             = "${pkgs.tree-sitter-grammars.tree-sitter-tiger}/parser";
      "nvim/parser/tlaplus.so".source           = "${pkgs.tree-sitter-grammars.tree-sitter-tlaplus}/parser";
      "nvim/parser/toml.so".source              = "${pkgs.tree-sitter-grammars.tree-sitter-toml}/parser";
      "nvim/parser/tsq.so".source               = "${pkgs.tree-sitter-grammars.tree-sitter-tsq}/parser";
      "nvim/parser/tsx.so".source               = "${pkgs.tree-sitter-grammars.tree-sitter-tsx}/parser";
      "nvim/parser/turtle.so".source            = "${pkgs.tree-sitter-grammars.tree-sitter-turtle}/parser";
      "nvim/parser/typescript.so".source        = "${pkgs.tree-sitter-grammars.tree-sitter-typescript}/parser";
      "nvim/parser/verilog.so".source           = "${pkgs.tree-sitter-grammars.tree-sitter-verilog}/parser";
      "nvim/parser/vim.so".source               = "${pkgs.tree-sitter-grammars.tree-sitter-vim}/parser";
      "nvim/parser/vue.so".source               = "${pkgs.tree-sitter-grammars.tree-sitter-vue}/parser";
      "nvim/parser/yaml.so".source              = "${pkgs.tree-sitter-grammars.tree-sitter-yaml}/parser";
      "nvim/parser/yang.so".source              = "${pkgs.tree-sitter-grammars.tree-sitter-yang}/parser";
      "nvim/parser/zig.so".source               = "${pkgs.tree-sitter-grammars.tree-sitter-zig}/parser";

      "nushell/config.nu".source     = ./config.nu;
      "nushell/env.nu".source        = ./env.nu;
      "wezterm/wezterm.lua".source   = ./wezterm.lua;
      "tridactyl/tridactylrc".source = ./tridactylrc.json;

      "deadd/deadd.css".text = ''
        /* Notification center */

        .blurredBG, #main_window, .blurredBG.low, .blurredBG.normal {
            background: rgba(255, 255, 255, 0.5);
        }

        .noti-center.time {
            font-size: 32px;
        }

        /* Notifications */

        .title {
            font-weight: bold;
            font-size: 16px;
        }

        .appname {
            font-size: 12px;
        }

        .time {
            font-size: 12px;
        }

        .blurredBG.notification {
            background:  rgba(255, 255, 255, 0.4);
        }

        .blurredBG.notification.critical {
            background: rgba(255, 0, 0, 0.5);
        }

        .notificationInCenter.critical {
            background: rgba(155, 0, 20, 0.5);
        }

        /* Labels */

        label {
            color: #322;
        }

        label.notification {
            color: #322;
        }

        label.critical {
            color: #000;
        }
        .notificationInCenter label.critical {
            color: #000;
        }


        /* Buttons */

        button {
            background: transparent;
            color: #322;
            border-radius: 3px;
            border-width: 0px;
            background-position: 0px 0px;
            text-shadow: none;
        }

        button:hover {
            border-radius: 3px;
            background: rgba(0, 20, 20, 0.2);
            border-width: 0px;
            border-top: transparent;
            border-color: #f00;
            color: #fee;
        }


        /* Custom Buttons */

        .userbutton {
            background: rgba(20,0,0, 0.15);
        }

        .userbuttonlabel {
            color: #222;
            font-size: 12px;
        }

        .userbutton:hover {
            background: rgba(20, 0, 0, 0.2);
        }

        .userbuttonlabel:hover {
            color: #111;
        }

        button.buttonState1 {
            background: rgba(20,0,0,0.5);
        }

        .userbuttonlabel.buttonState1 {
            color: #fff;
        }

        button.buttonState1:hover {
            background: rgba(20,0,0, 0.4);
        }

        .userbuttonlabel.buttonState1:hover {
            color: #111;
        }

        button.buttonState2 {
            background: rgba(255,255,255,0.3);
        }

        .userbuttonlabel.buttonState2 {
            color: #111;
        }

        button.buttonState2:hover {
            background: rgba(20,0,0, 0.3);
        }

        .userbuttonlabel.buttonState2:hover {
            color: #000;
        }


        /* Images */

        image.deadd-noti-center.notification.image {
            margin-left: 20px;
        }
        '';
    };
  };

  # Polybar
  # https://pbrisbin.com/posts/xmonad_statusbars/
  # https://github.com/xintron/xmonad-log
  # https://hackage.haskell.org/package/xmonad-contrib-0.15/docs/XMonad-Hooks-ManageDocks.html
  # https://github.com/jaagr/polybar/wiki/User-contributed-modules

  # Example dotfiles:
  # polybar + xmonad | https://github.com/idzardh/dotfiles
  #                    https://idzardblog.wordpress.com/2017/09/17/xmonad-polybar/<Paste>
  # https://github.com/bobvanderlinden/nix-home/blob/master/home.nix
  # https://github.com/jagajaga/my_configs/blob/master/.nixpkgs/common.nix
  # https://github.com/andrewrk/dotfiles/blob/master/.nixpkgs/config.nix
  # https://github.com/lo1tuma/nix-config/blob/master/config/default.nix
  # https://www.reddit.com/r/NixOS/comments/9bb9h9/post_your_homemanager_homenix_file/
  # https://github.com/dustinlacewell/dotfiles
  # https://github.com/pkinsky/niXmonad/blob/master/configuration.nix
  # https://gist.github.com/domenkozar/b3c945035af53fa816e0ac460f1df853

  # Misc config stuff to trawl
  # https://nixos.wiki/wiki/Home_Manager#Examples
  # https://github.com/jondot/awesome-devenv
  # https://github.com/zplug/zplug
  # https://github.com/rafi/awesome-vim-colorschemes
  # https://vaibhavsagar.com/blog/2018/05/27/quick-easy-nixpkgs-pinning/
  # https://github.com/daviwil/dotfiles/tree/7ed5195e0007bccb43420cfec271ab779f4720fd
  # https://config.daviwil.com/
  # http://chriswarbo.net/projects/nixos/useful_hacks.html
  # Check config for other zsh modules
  # https://dougblack.io/words/zsh-vi-mode.html
  # https://terminalsare.sexy/
  # Check config for various vim plugins

  # TODO: write a copy tool (tentatively) called "telegraph" that can copy something from stdin in
  #       one terminal, then paste it to stdout in another, optionally using a handle for said
  #       thing (and potentially optionally keeping said thing open). It would detect whether it's
  #       receiving on stdin or not, and adapt its behaviour correspondingly.
  #
  #       Example:
  #       Terminal 1:
  #         telegraph < some.file
  #       Then, in terminal 2:
  #         telegraph > file.exists.here.now
  #
  #       Example with handle
  #       Terminal 1:
  #         telegraph nginx_config_file < nginx.conf
  #       Then, in terminal 2:
  #         telegraph nginx_config_file > nginx.conf
  #       Bonus points for autocomplete here
  #
  #       Bonus points for
  #       - streaming the file without loading it all into memory first, probably through a named
  #         fifo (in /run or similar? where do these things normally go?).
  #       - supporting directories
  #       - supporting move (not just copy)
  #       - supporting multiple files
  #         - this could potentially be achieved by having multiple fifos in a directory e.g.
  #           /run/telegraph/default/file1 /run/telegraph/default/file2 etc.
  #
  #       Notes:
  #       - telegraph should use a default named fifo, and delete it after it's finished- then it
  #         can detect if that default named fifo already exists and present an error ("another
  #         telegraph is being sent on this wire right now").
  #
  #       Options:
  #         -f, --fork     | run in the background, this allows the user to navigate elsewhere in
  #                        | the same terminal then call telegraph when they've reached their
  #                        | destination
  #         -m, --multiple | allow the input to be consumed multiple times (this might be too
  #                        | difficult to achieve cleanly, and be a niche use case anyway)
  #
  # TODO: polybar is a combination of state + presentation
  #       - have some sort of state/monitoring service that records a range of system information
  #       - make the status bar a simple presentation layer on top of that information
  # TODO: add a (moving average?) ping to polybar as a rough gauge of internet connectivity.
  #       Perhaps just have a range, like <300ms green, 300-1000ms orange, >1000ms red?
  # TODO: implement complete tab sync for FF profiles- this might mean syncing the whole profile
  #       directory, or database?
  # TODO: move all youtube usage to freetube, sync freetube conf etc. via GH. And/or move YT
  #       bookmarks to buku, with tags
  # TODO: how/can I bandwidth limit a single command? (In particular, sometimes this would be very
  #       convenient to do when running a full system update).
  # TODO: man configuration.nix programs.firejail
  # TODO: wrap cd so I can cd to a file and it'll transparently cd $(dirname $file)
  # TODO: any reference to $HOME/.dotfiles or ${config.home.homeDirectory}/.dotfiles in any file in
  #       this repo should probably be replaced with $DOTS. This way the dotfiles repo can be moved
  #       around without (or with less..) error.
  # TODO: drop-down terminal; should be possible (who cares about animations) without some
  #       third-party package by using alacritty --classname and handling that WM_CLASSNAME
  #       correctly in XMonad config. The question is: one per workspace?
  # TODO: Editor wishlist:
  #       - embeddable, to enable e.g. a language-agnostic command line with all the bells and whistles of a real editor
  #       - an editor library that you combine yourself (like XMonad is to window managers)
  #         - Reasonably fast compile times
  #         - This is feasible in XMonad because types tell users a lot about the interface and
  #           capabilities they can leverage. Rust has a rich type system that will give the same
  #           advantages.
  #           - It's rather tricky to configure XMonad as a non-Haskell user
  #         - This is good because users have extreme control over their editor.
  #       - libraries for:
  #         - native LSP integration (what does this mean? what is the desired functionality?)
  #           - autocomplete
  #           - code suggestions
  #           - compile/analysis/lint error information
  #         - native treesitter integration
  #           - textobjects
  #           - syntax highlighting
  #           - auto-bracket pairs
  #         - native jump motions
  #         - "virtual text"
  #       - client-server architecture:
  #         - should reduce the trouble of editing the same file in multiple windows
  #         - remove langserver start-up times (i.e. rust-analyzer seems to require some time to
  #           start up)
  # TODO: make a mullvad vpn + bubblewrap (the sandboxing bubblewrap) integration called moleskin.
  #       Just for the name, really, which is a play on bubblewrap (skin) and Mullvad (mole). But
  #       anyway, generally allow the user to sandbox something on-demand with bubblewrap and have
  #       all data go through an ephemeral Mullvad connection.
  # TODO: what is xsuspender?
  # TODO: Add all git subcommands as commands when inside a git repo
  # TODO: Add all cargo subcommands as commands when inside a cargo project
  # TODO: better notification center
  #       - https://wiki.archlinux.org/title/Desktop_notifications
  #       - consider just writing a GUI/TUI for dunstctl history (and increasing history length to
  #         infinite, and filtering history)
  #       - https://github.com/rfjakob/systembus-notify
  #       - deadd
  #         - https://github.com/phuhl/linux_notification_center
  #         - https://github.com/Mesabloo/nix-config/blob/master/extra/nix-overlays/packages/deadd-notification-center.nix
  #         - https://github.com/phuhl/linux_notification_center/issues/63
  #         - https://github.com/NixOS/nixpkgs/pull/113222/files
  # TODO: secret management
  #       - https://nixos.wiki/index.php?title=Comparison_of_secret_managing_schemes&useskin=vector#Comparison
  # TODO: get some ideas from here: https://old.reddit.com/r/unixporn/top/?sort=top&t=all
  # TODO: can I (easily) configure my keyboard to only highlight currently allowed keys?
  #       - https://old.reddit.com/r/unixporn/comments/hgba3b/i3_razer_blade_stealth_highlighting_shortcuts_and/
  # TODO: https://wiki.archlinux.org/title/Browser_extensions#Edit_text_with_external_text_editor
  # TODO: sandbox stuff. Lots of stuff.
  #       - https://xeiaso.net/blog/paranoid-nixos-2021-07-18
  #       - https://nixos.wiki/wiki/Security (make sure to read various links and references)
  #       - https://wiki.archlinux.org/title/Firejail
  #       - https://wiki.archlinux.org/title/Bubblewrap
  #       - https://mullvad.net/en/help/split-tunneling-with-the-mullvad-app/#linuxapp
  #       - AppArmor
  #       - SELinux
  #       - pledge.com
  # TODO: native:
  #       - e-mail client
  #       - chat client
  # TODO: understand https://wiki.archlinux.org/title/Polkit#Authentication_agents
  #       - https://nixos.wiki/wiki/Polkit
  # TODO: system monitoring
  #       - what's using CPU, therefore what should I focus on optimising/removing?
  #       - what's running? For example, Signal seems to occupy much more CPU than it should, but
  #         to identify that, we need to know (1) that it's running and (2) that it's using CPU. If
  #         we see that it's got high CPU usage 10% of the time, but it's not running the rest of
  #         the time, we won't know to stop it, or replace it with an alternative
  #       - https://github.com/facebookincubator/below
  #       - https://wiki.archlinux.org/title/Monitorix
  #       - https://wiki.archlinux.org/title/Lm_sensors
  #       - https://wiki.archlinux.org/title/List_of_applications/Utilities#System_monitors
  # TODO: Pipewire
  #       - https://github.com/wwmm/easyeffects
  # TODO: https://github.com/rothgar/awesome-tuis
  # TODO: more wallpapers:
  #       - https://wallpapercave.com/categories/nature
  #       - https://wall.alphacoders.com/by_sub_category.php?id=168652&name=Mountain+Wallpapers&filter=4K+Ultra+HD
  #       - https://wall.alphacoders.com/tag/4k-mountain-wallpapers
  # TODO: translation of current x selection. Could pass current x selection to crow.
  #       - https://xmonad.github.io/xmonad-docs/xmonad-contrib/XMonad-Util-XSelection.html
  # TODO: - https://wiki.archlinux.org/title/Xmonad#Controlling_xmonad_with_external_scripts
  #       - https://wiki.archlinux.org/title/Xmonad#Tips_and_tricks
  # TODO: power management, in particular reduce power consumption
  #       - https://wiki.archlinux.org/title/Power_management
  #       - https://wiki.archlinux.org/title/Laptop#Power_management
  #       - https://wiki.archlinux.org/title/Dell_XPS_15_9570
  #       - Can we disable wifi scanning when connected? I.e. manually connect only when we're
  #         already connected to a network?
  #       - https://www.kernel.org/doc/html/latest/admin-guide/cgroup-v1/freezer-subsystem.html
  #         - is this available via systemctl? can we identify which cgroup corresponds to a
  #           systemd service? Yes:
  #           - https://www.freedesktop.org/software/systemd/man/systemctl.html#freeze%20PATTERN%E2%80%A6
  #           - https://www.freedesktop.org/software/systemd/man/systemctl.html#thaw%20PATTERN%E2%80%A6
  #           And it trivially works. Wow.. Awesome. Can we... suspend services to disk??
  #       We should be able to use XMonad window focus hooks to freeze/thaw various services when
  #       they lose/gain focus respectively. For example, whatsapp, signal, email, etc. don't need
  #       any CPU time when they're not focused. Perhaps we could check whether we're on AC power
  #       and above 80% battery, if we are :shrug:, but if we aren't, freeze the service.
  # TODO: https://wiki.archlinux.org/title/Hybrid_graphics
  #       - https://wiki.archlinux.org/title/Dell_XPS_15_9570#Graphics
  # TODO: make a generalised tree select tui or gui, where the user provides a tree of options in
  #       some input format, and the tui/gui allows them to select a leaf item.
  #       - allow single-key mode, so that the user provides each node with a selection key
  #         - this allows fast navigation for users accustomed to a menu they use often
  #       - allow descriptions, or perhaps even unstructured metadata
  #       - allow the user to specify how much of the tree is displayed, e.g. whether the parent
  #         list and the currently selected child list should be displayed (so in this example,
  #         three levels, parents, the node level and the children)
  #       - optional title
  #       - modular design so that display and navigation are separate, such that it's easy to make
  #         both tui and gui
  #       - styling configuration
  #       - key-binding configuration
  #       - fuzzy select/filter
  #         - optional auto-select when only a single option remains
  #       - use json or ron (rust object notation)
  #       - lazy data generation, such that if a user selects/views an item a shell is spawned to
  #         return its children
  #         - optional caching
  #       - optionally return either the node, or the path through the tree?
  #       - deep-tree search functionality, allowing fuzzy search on the whole tree at once, i.e.
  #         if a node five levels deep matches, display this node
  #       - this is fundamentally a fairly simple data structure, the display is orthogonal *but*
  #         there could well be some helper methods on nodes, for example, to
  #         - traverse the tree
  #         - generate the currently allowable keys etc. depending on the configuration
  #         - validate the configuration
  #         - generate the display data depending on the configuration (i.e. title, description,
  #           panels)
  #       - how to handle multi-select?
  #       - could this be built from shell (e.g. bash) completions, such that the user could
  #         navigate the available options for a command using this tool? (See fig completions,
  #         which are user-contributed and open source: https://github.com/withfig/autocomplete)
  #       - could this accept unstructured JSON data?
  #         - JSON is, after all, a fairly simple data format with a tree structure
  #         - could it be used to formulate a JSON query for later use? For example, the traversal
  #           information, e.g. the selected item in an array, the key of a given object, etc.,
  #           could be used to create the query. The output could be e.g. jq:
  #             '.a.b.[] | select(.id == "blah")'
  #           or some sort of traversal query native to this tool
  #       - show the selection so far in a bar at the top, e.g.
  #           systemctl -> --user -> status -> gmail.service
  #       - https://github.com/veeso/tui-realm
  #       - https://xmonad.github.io/xmonad-docs/xmonad-contrib/XMonad-Actions-TreeSelect.html
  #       - show some sort of preview pane for the current selection? E.g. if creating a systemd
  #         manager, it would be useful to show the currently selected service in a preview pane,
  #         even before selecting the action we'd like to take with it.
  #       A node likely looks something like this:
  #         Node {
  #           id: String;
  #           parent: Node;
  #           description: Option<String>;
  #           select_key: Option<KeySym>;
  #           children: Either<Vec<Node>, NodeFetcher>;
  #         }
  #       And likely has methods
  #         getHandleKeys() -> Vec<KeySym> # (probably not required, at least at first)
  #         handleKey(KeySym) -> Node
  #         getDisplay() -> Display {
  #           panels: Vec<Vec<String>>;
  #           description: Option<String>;
  #           title: Option<String>;
  #         }
  # TODO: move all bookmarks to a local file of some sort, then use rofi to search them, and
  #       select-browser to open them. Then have a bookmarkless, historyless, stateless (maybe,
  #       though consider e.g. Github, StackOverflow) browser.
  # TODO: terminal state saving. State saved when the terminal is exited via a specific signal
  #       (probably issued when the machine is shut down). Or saved regularly during use. Depending
  #       on user preference. Interesting use cases:
  #       - reload all terminals that were open when the machine was shut down
  #       - reload all terminals that were open in (or below) a certain directory (a certain
  #         project) - optionally to the workspace they were open on when they were closed
  #       - reload all terminals that were open on a certain X workspace (before I sent
  #         SIGTERMCLOSEANDSAVESTATE to every window on that workspace)
  #       - reload all terminals with a certain x window property value (e.g. a certain window tag)
  #       State to save (easy mode):
  #       - workspace the terminal window is on
  #       - current x window props (in case it's been tagged by the window manager etc.)
  #       - currently open directory
  #       - files open in editor
  #       - terminal history buffer
  #       Hard mode might be way easier than expected; it *might* be possible to use the kernel
  #       freeze functionality to serialize the processes to disk. Note that it says "processes can
  #       be checkpointed" and "move to another node" which could be the same node at some time in
  #       the future:
  #       - https://www.kernel.org/doc/html/latest/admin-guide/cgroup-v1/freezer-subsystem.html
  #       References/notes:
  #       - see xprop _NET_WM_PID (not much use in the case of a terminal *client*)
  #       - /proc/$PID/cwd
  #       - /proc/$$/cwd
  #       - /proc/$PPID/cwd
  #       - other stuff in /proc
  # TODO: pueue + notify-send / dbus integration; then put nixos-rebuild update into pueue?
  # TODO: check out Mullvad split tunneling. Is it possible to rename some binaries on the fly to make split tunnelling easier?
  # TODO: encrypted RAM? Possible? Useful? The key has to go somewhere... Which probably means I'd
  #       need some sort of hardware support.
  # TODO: command that opens dotfiles in broot? df?
  # TODO: hotkey to open terminal straight to `notes` and `tv` commands. I.e.
  #       - alacritty -e zsh -ic "notes"
  #       - alacritty -e zsh -c "broot -i ~/.dotfiles/notes"
  #       Note that the first of these is an interactive shell and is noticeably slower to load
  # TODO: insert githubnotifications.token into polybar config using nix file import- this way if
  #       it's missing the config will fail to build. _But_ may have to do this in such a way that
  #       the flake lock doesn't require it?
  # TODO: turn init.vim and xmonad.hs (and any other config files) into nix expressions (even if
  #       only strings) in order to directly reference packages with string interpolation
  # TODO: `mutableUsers = false`
  #       - https://nixos.org/manual/nixos/stable/options.html#opt-users.mutableUsers
  #       - https://nixos.org/manual/nixos/stable/#sec-user-management
  # TODO: systemd service + timer for automatic notes syncing. See what the password-store service
  #       + timer does (git pull --rebase?).
  #       - https://github.com/GitJournal/git-auto-sync
  # TODO: move `tv` notes to same repo as general notes? Or to their own repo? Then create a
  #       systemd service + timer to auto-sync. See what the password-store service + timer does
  #       (git pull --rebase?).
  #       - https://github.com/GitJournal/git-auto-sync
  # TODO: look at xidlehook, xsettingsd and xsuspender in man home-configuration.nix
  # TODO: `pass` and GitJournal have nice auto-update mechanisms. Perhaps this could be
  #       replicated with local note taking using the GitJournal repo, but also the dotfiles notes,
  #       which I often give pretty meaningless commit messages to
  # TODO: Vim plugin to replace the URL beneath the cursor with an archive.org archive
  # TODO: translations for Firefox with hover results, preferably using a local translation model
  #       (although presumably if this was easy there wouldn't be an EU-funded Firefox translation
  #       project)
  # TODO: ergonomic password/passphrase generator. Yes, this will reduce entropy, but in exchange
  #       the user could have a longer password/passphrase that prioritises alternating hands on
  #       the keyboard, for example, or words that are easier to type (don't require the pinky,
  #       etc.). See: https://news.ycombinator.com/item?id=31021888
  # TODO: we can build in an ability to use AOT compiled languages as scripts in nix. It will
  #       require a system rebuild (which will trigger a compilation of our "scripts"), but so does
  #       e.g. changing .zshrc.
  # TODO: a basic piece of software that scans for wireless networks that are already configured
  #       and presents a list for the user to select which to connect to. Probably could do
  #       something like:
  #         wifi_ssid_scan | skim | -xargs -I{} wpa_cli select_network {}
  #       Specifically, the tool just needs to perform a (synchronous) scan, then output the list
  #       of networks. Other tools can do the rest of the job.
  #       See: https://stackoverflow.com/a/16752895
  #       Note this command does what we want but the output is difficult to use (sudo required):
  #         sudo iwlist scan 2>/dev/null | grep ESSID
  #       Could generally use a nicer interface for connecting to ephemeral wifi with NixOS +
  #       wpa_supplicant. _Probably_ this involves having a separate wifi.nix and appending to it
  #       or amending it with new pskRaw etc., then having the user update their system.
  # TODO: strace explainer tool; basically automating this:
  #       https://stackoverflow.com/questions/6334515/how-to-interpret-strace-output
  #       - first iteration could just be easy buttons to expand the relevant man pages; i.e.
  #         transform your strace log (just a list of syscalls) into an HTML page with the ability
  #         to expand/contract the particular line into an explainer
  #       - later, specific files could be called out, e.g. a call like this:
  #           newfstatat(AT_FDCWD, "/nix/store/8kgsjv57icc18qhpmj588g9x1w34hi4j-bash-interactive-5.1-p12/bin/sudo", 0xc000ae2518, 0) = -1 ENOENT (No such file or directory)
  #         could be explained in a summary as saying something like "normally fstatat is looking
  #         for the presence of a particular file; in this case, because it's looking for the sudo
  #         file, it's probably checking whether sudo exists on your machine and may later check
  #         whether you have access to it"
  # TODO: network namespacing of processes: https://github.com/NixOS/nixpkgs/pull/71510
  # TODO: create network namespaces for each Mullvad exit node, so that it's possible to use `ip
  #       netns exec mullvad-gb-manchester su - msk` for e.g. to get a shell where all network
  #       traffic exits from the Manchester Mullvad node. Should be able to script creation of
  #       these. See:
  #       - https://github.com/NixOS/nixpkgs/pull/71510
  #       - https://github.com/NixOS/nixpkgs/issues/52411
  #       - https://lib.rs/crates/vopono
  # TODO: Tor network namespace, so we can use `ip netns exec tor su - msk`.
  #       See: https://github.com/orjail/orjail
  # TODO: cargo plugin, `cargo manifest` to walk the directory tree upward and open the first found
  #       Cargo.toml file in $EDITOR.
  # TODO: check out https://wiki.archlinux.org/title/Vopono
  # TODO: kubernetes port-forward manager, cli (+ druid-gui?)
  #       - is this a special case of bridge to kubernetes? (but without VSCode)
  # TODO: convert vim config to lua
  # TODO: turn off zsh shared history
  # TODO: handle URLs with a piece of software other than a browser. When a Github URL to source is
  #       detected open it in a terminal, in vim, instead of in the browser.
  # TODO: alias cp is annoying when it's expanded globally. Is it possible to use alias -m '^cp' to
  #       prevent that?
  # TODO: having the BROWSER variable create a new profile in /tmp every time the browser is
  #       started causes it to take a while to load and fill up the /tmp directory. Do something
  #       about this? Some sort of browser wrapper that deletes its created profile after running
  #       if said profile is in /tmp ? wrapProgram and/or systemd-tmpfiles could achieve this.
  # TODO: clear out /tmp periodically
  # TODO: vim jump-to-github from code line
  # TODO: configure BT earbuds to mute/unmute (in Pulseaudio?) when one of the headphone buttons is pressed
  # TODO: reminder utility like the one I wrote in the past
  # TODO: get chromecast audio sink for pulseaudio
  # TODO: load dictionary into vim autocomplete when working on markdown files? (or would that just
  #       be annoying?)
  # TODO: Spotify control hotkeys
  # TODO: check out `hub` CLI
  # TODO: airplane mode? (rfkill??)
  # TODO: is it possible to stop Zoom from receiving window unfocus events? That way it might stay
  #       full-screened on whichever workspace it started on, like I'd prefer, instead of popping
  #       out and floating.
  # TODO: get that version of `man` with the examples
  # TODO: turn on vsync? should stop tearing when scrolling browser and watching videos
  # TODO: build a terminal in desktop flutter?
  # TODO: terminal + vim have switch between dark and light mode to make working in bright/dark
  #       environments easier.
  # TODO: system-wide microphone amplitude cut-off (like Mumble and other talk apps).
  # TODO: hide firefox chrome? Or at least address bar. Maybe tabs (but then I'd have to keep my
  #       number of tabs under control).
  # TODO: XMonad: "send this current window to the workspace occupied by that other window" via
  #       dmenu.
  # TODO: ephemeral terminal with `tv` options popped open so I can call `tv` directly from dmenu
  #       and have it disappear immediately after I close it.
  # TODO: disable mouse buttons other than left, right and middle/scroll.
  # TODO: clean up home directory. Stupid software that puts stuff there rather than ~/.config.
  #       Some of it might have env vars or options that prevent that.
  # TODO: add git information to prompt
  #       dupe: display current git branch (+status?) in terminal prompt
  # TODO: https://en.wikipedia.org/wiki/Magic_SysRq_key
  #       https://linuxconfig.org/how-to-enable-all-sysrq-functions-on-linux
  # TODO: auto-suspend on low battery
  # TODO: hotkey for muting and unmuting all microphone inputs, and a status bar indication of
  #       whether it's on or off
  # TODO: how can I easily (preferably interactively) remove stuff from my command history?
  # TODO: how can I easily (preferably interactively) remove stuff from my directory history?
  # TODO: local dns server + caching + ad-blocking
  # TODO: Use fzy or some sort of fuzzy-searcher (perhaps there's a Haskell-native one) for
  #       goToWindow- just to make it less sensitive to typos. Could even write my own substring
  #       matcher, or native XMonad implementation.
  # TODO: At _least_ make some sort of popup notification for low battery!
  # TODO: Loading kubectl completions on every shell start (at the end of .zshrc) is slow. Is it
  #       better to package these (with nix) and append them to zshrc?
  # TODO: (pertaining somewhat to the above TODO) why is the terminal slow to open? Because of
  #       things like loading completions? It breaks my concentration; try to eliminate it.
  # TODO: reminder framework that integrates with multiple devices and has cli. Perhaps a Keybase
  #       app?
  # TODO: if a sequence of subdirectories contains nothing, autocomplete to the depth of the first
  #       non-directory file, or fork in the tree.
  #       For example if the file ./some/directory/sequence/file.ext exists, but there are no files
  #       in parent directories, `cd` autocomplete should autocomplete the entire sequence.
  # TODO: put zoom, and meetings, on a specific workspace?
  # TODO: Drop-down terminal that disappears after a forked command? Do I really want this? Replace
  #       dmenu with this?
  # TODO: it's possible for the system to come out of hibernate and not be locked. This shouldn't
  #       be a problem, because the system state is saved to swap, which is encrypted and password
  #       protected. But it's worth thinking about whether this is a problem; what if I install
  #       this system to another machine without disk encryption?
  # TODO: change all notes to markdown? Just set vim opts ft=md at the end? Editorconfig?
  # TODO: can I blacklist domains in my browser so that I see links to them in black- indicating
  #       that they're terrible sites I never want to visit? I.e. yummly. Similarly: search result
  #       blocker browser add-ons.
  # TODO: some sort of text input in xmonad so I can type a search from anywhere and be taken to my
  #       browser, where that search/URL is executed- or better, an ephemeral browser instance
  #       without history etc.
  # TODO: mic mute/unmute hotkey
  # TODO: wrap chromium with wrapProgram to enforce incognito, and set GDK_DPI_SCALE?
  #       | https://nixos.wiki/wiki/Nix_Cookbook#Wrapping_packages
  # TODO: auto-suspend at a certain battery level
  # TODO: figure out how to turn off all radios etc. for flight mode
  # TODO: tridactyl
  #       | replace Hide Fixed Elements browser extension with a Tridactyl script; inspect the Hide Fixed Elements code to see if it does anything other than add a css rule)
  #       | option to grayscale page when following hint. I.e. pressing f temporarily grays the page. This would mean using nicer colours for hints would be more feasible, as they wouldn't clash with pages.
  #       | hide fixed elements
  #       | enter/exit reader mode
  #       | tridactylrc (search man home-configuration.nix for tridactyl native support)
  #       | guiset/userChrome.css to control the chrome
  # TODO: change prompt to show a) git branch b) whether there is anything in the git stash c)
  #       whether there are unstaged changes/uncommitted changes/untracked files/unpushed commits
  # TODO: can I wrap the chromium binary to use a different profile every time? Or the --incognito flag?
  # TODO: can I change the matching opening bracket in nix files to also have a semicolon after it?
  # TODO: journalctl --user -xm
  #       | Feb 24 18:11:55 nixos mpd[1575]: exception: Failed to access /home/msk/music: No such file or directory
  #       | Feb 24 18:11:55 nixos mpd[1575]: output: No 'AudioOutput' defined in config file
  #       | Feb 24 18:11:55 nixos mpd[1575]: output: Attempt to detect audio output device
  #       | Feb 24 18:11:55 nixos mpd[1575]: output: Attempting to detect a alsa audio device
  #       | Feb 24 18:11:55 nixos pulseaudio[1614]: E: [pulseaudio] module-jackdbus-detect.c: Unable to contact D-Bus session bus: org.freedesktop.DBus.Error.NotSupported: Unable to autolaunch a dbus-daemon without a $DISPLAY for X11
  #       | Feb 24 18:11:55 nixos pulseaudio[1614]: E: [pulseaudio] module.c: Failed to load module "module-jackdbus-detect" (argument: "channels=2"): initialization failed.
  #       | Feb 24 18:11:55 nixos pulseaudio[1614]: W: [pulseaudio] server-lookup.c: Unable to contact D-Bus: org.freedesktop.DBus.Error.NotSupported: Unable to autolaunch a dbus-daemon without a $DISPLAY for X11
  #       | Feb 24 18:11:55 nixos pulseaudio[1614]: W: [pulseaudio] main.c: Unable to contact D-Bus: org.freedesktop.DBus.Error.NotSupported: Unable to autolaunch a dbus-daemon without a $DISPLAY for X11
  #       | Feb 24 18:11:55 nixos libsmbclient[1575]: avahi: Failed to create avahi EntryGroup: Not permitted
  #       | Feb 24 18:11:55 nixos libsmbclient[1575]: exception: OutputThread could not get realtime scheduling, continuing anyway: sched_setscheduler failed: Operation not permitted
  #       | Feb 24 18:11:55 nixos setxkbmap[1658]: Error loading new keyboard description
  #       | Feb 24 18:11:55 nixos systemd[1568]: setxkbmap.service: Main process exited, code=exited, status=251/n/a
  #       | Feb 24 18:11:55 nixos systemd[1568]: setxkbmap.service: Failed with result 'exit-code'.
  #       | Feb 24 18:11:55 nixos systemd[1568]: Failed to start Set up keyboard in X.
  #       | Feb 24 18:11:55 nixos libsmbclient[1575]: exception: Failed to read mixer for 'default detected output': no such mixer control: PCM
  # TODO: option to disable non-vpn protected connections
  # TODO: expose a DO droplet VPN spin-up as a service in my system?
  # TODO: update fetchFromGitHub packages
  # TODO: is it possible to only add specific binaries to the PATH? Why do I have 'aconnect' in my
  #       path? Would need some way to find out what package provides a given binary, though, so
  #       that it would be easier to identify if it was already installed but not exposed.
  # TODO: put .ignore into home directory (or possibly integrate it into ag, or put it in the
  #       ~/.config dir) as part of home-manager build
  # TODO: after exiting dmenu, xmonad reverts focus to the next window in the stack, rather than
  #       the last selected window. Is it possible to change this behaviour?
  # TODO: put all the electron/"chrome apps" into a cgroup with a shared memory/cpu pool instead of
  #       restricting each individually
  # TODO: fix alacritty config to set the cursor style/shape correctly when it regains focus. I.e.
  #       after pressing <M-o> and returning to alacritty; the cursor is block-outline, even in
  #       insert mode.
  # TODO: use WM_CLASS and WM_NAME in dmenu, xmonad goToWindow- that way we can always find Spotify
  # TODO: search zshrc for all xterm/rxvt-specific functionality and remove/replace it
  # TODO: some sort of space background image? Or landscapes?
  # TODO: alacritty seems to advertise itself as xterm-256color. Or have I set a setting somewhere
  #       to do that? Is it correct/sensible behaviour? (probably, so certain applications will
  #       support it).
  # TODO: fix cursor size
  # TODO: toggle automatic screen-off for watching movies
  # TODO: VIM: consider a key toggling a 'verbatim' text entry mode in insert mode, so that
  #       inserting an opening bracket does not automatically insert a closing bracket, etc. (Just
  #       using paste mode might work?). And/or have a more convenient delete in insert mode;
  #       perhaps like shift+BS or something? (because ctrl+BS is mapped to delete previous word in
  #       most conventional text entry). And/or see if whatever auto-pair-entry thing I use
  #       supports a key mapping for removing the pair it just inserted?
  # TODO: VIM: make a nicer keymapping for "next, previous difference" (currently `]c`, `[c`).
  #       Consider for example ;j and ;k (I don't use semicolon).
  # TODO: when the pointer comes out of hiding, try to make it extra visible. Perhaps do the mac
  #       thing where shaking the pointer makes it larger.
  # TODO: consider moving boot partition to USB drive
  # TODO: make sure the discrete graphics card is switched off
  # TODO: enable alt+sysrq? (how to use on a laptop?) Any other really low-level interrupts?
  # TODO: power conservation. Search something like "dell xps 15 linux power usage" or just
  #       "linux laptop power usage". Also, check I got the 97WHr battery I ordered.
  # TODO: Debug/diagnose: "package temperature above threshold, CPU throttled" messages (are they
  #       visible in journalctl?)
  # TODO: BIOS error, switch to a VT not running X, do not log in, modify the backlight. Does an
  #       error print? If not, this problem has probably been resolved by somebody. If so, what's
  #       going on?
  # TODO: set up dnscrypt-proxy once it's updated past v2 in nixpkgs.
  #       https://developers.cloudflare.com/1.1.1.1/dns-over-https/cloudflared-proxy/
  #       - if possible use the ipv6 cloudflare resolvers; as the data from them may not be shared with APNIC
  #       https://nixos.org/nixos/manual/#sec-dnscrypt-proxy
  #       https://dnsleaktest.com/ (add result to status bar?)
  #       https://github.com/aarond10/https_dns_proxy
  #       https://github.com/NixOS/nixpkgs/issues/43298
  #       https://github.com/jedisct1/dnscrypt-proxy/wiki
  # TODO: set up wireguard interfaces to switch between vpn servers
  # TODO: cache dns query results? Does dnscrypt-proxy do this? (Yes, according to docs)
  # TODO: is it possible to sandbox processes more stringently? At a processor level? I.e., can I
  #       create a fairly minimal virtualised chromium, for example? Is it worthwhile? Do/can
  #       cgroups afford me most of the benefits with lower costs (effort)?
  # TODO: investigate how to shut vim/emacs down cleanly upon shutdown (or if emacs were a
  #       service..). Is it possible to send a signal requesting clean shutdown, and fail to
  #       shutdown if that's not an option (i.e. if a file has unsaved state)?
  # TODO: update bios/firmware
  # TODO: verify ACPI is working; this can have a significant effect on battery life
  #       nix-shell -p acpi --run "acpi -V"
  # TODO: alacritty terminfo
  # TODO: is it worth network whitelisting and file-system restricting certain processes?
  # TODO: password manager
  # TODO: theme ff? https://github.com/horst3180/arc-firefox-theme
  # TODO: document firefox addons somewhere?
  #       | https://github.com/NixOS/nixpkgs/issues/15959
  # TODO: for work, email client supporting pgp
  # TODO: it's possible to use different network stacks for different processes. Investigate
  #       whether this could be useful. E.g. some processes could be behind VPN by default, and
  #       disabled if it's not running.
  # TODO: if possible set up systemd to put certain processes in a network-disabled cgroup. E.g.
  #       vim; suppose one of my plugins was compromised. Have a look at other installed software
  #       and see where this is sensible/feasible.
  # TODO: use systemd IPAddressAllow and IPAddressDeny to network-restrict chromium app processes.
  #       E.g. restrict all the google services to only access google addresses. Signal to only
  #       signal, etc.
  # TODO: possible to isolate some processes with nix containers? https://nixos.org/nixos/manual/#ch-containers
  # TODO: get work calendar on personal calendar?
  # TODO: power management | https://github.com/NixOS/nixos/blob/master/modules/config/power-management.nix
  # TODO: i18n (but might be doable in home manager) | https://github.com/NixOS/nixpkgs/blob/master/nixos/modules/config/i18n.nix
  # TODO: backlight | https://nixos.wiki/wiki/Backlight
  # TODO: kexec-reboot | https://github.com/error10/kexec-reboot
  #                    | https://en.wikipedia.org/wiki/Kexec
  #                    | https://www.google.com/search?q=nixos%20kexec-reboot
  # TODO: ensure M-sysrq is enabled (and check it works): https://lifehacker.com/298891/gently-restart-a-frozen-system
  # TODO: investigate https://github.com/kana/vim-arpeggio. Could poss use jf keys simultaneously
  #       for exiting insert mode? Would mess with habit though, and be annoying if zsh didn't
  #       support. (Get vim-readline as below..?).
  # TODO: try out vim-readline: https://github.com/ardagnir/athame
  # TODO: check out NUR: https://github.com/nix-community/NUR
  # TODO: wireguard | https://nixos.wiki/wiki/Wireguard
  # TODO: put all of the chromium processes in the same cgroup? Have them use the same resource
  #       pool. Then start all the google apps from the same chrome profile. Put them in the same
  #       network namespace? https://www.freedesktop.org/software/systemd/man/systemd.exec.html#NetworkNamespacePath=
  # TODO: put spotify in its place https://hackage.haskell.org/package/xmonad-contrib-0.15/docs/XMonad-Hooks-DynamicProperty.html
  # TODO: how are calendar, gmail etc. maintaining cookies?! Figure out how to install them such
  #       that they have all the chromium plugins I've specified. NOTE: they seem to actually be
  #       storing profiles in /home/msk/\$HOME/.config/... See the contents of home dir for more.
  #       A fix for this might be to get from environment as here:
  #       https://git.sr.ht/~ben/config/tree/master/common.nix
  # TODO: systemctl [--user] status; check the system isn't running degraded
  # TODO: put systemctl [--user] status in the status bar
  # TODO: https://nixos.org/nixos/manual/options.html#opt-services.logind.lidSwitchDocked
  # TODO: consider https://github.com/geommer/yabar
  # TODO: read nix pills https://nixos.org/nixos/nix-pills/
  # TODO: read manual: https://nixos.org/nix/manual/
  # TODO: programs.direnv.enable = true; # https://github.com/direnv/direnv/wiki/Nix
  # TODO: programs.noti.enable = true;
  # TODO: programs.taskwarrior.enable = true; # Or some equivalent
  # TODO: systemd user service autorestart
  # TODO: use fzy for tab-autocompletion for zsh. See the vim-fzy vim plugin above for perhaps a
  #       solution (just bind autocomplete key to one of the widgets that supplies?) or
  #       inspiration.
  # TODO: consider a move to emacs
  # TODO: enable alt+sysrq (?) interrupt? And C-M-Backspace?
  # TODO: https://github.com/alols/xcape
  # TODO: because some applications don't really handle 4k that well, and especially because they
  #       shrink the cursor, install something like https://github.com/Carpetsmoker/find-cursor.
  #       Additionally, possible fork the unclutter service to allow the user to supply something
  #       to exec when it hides/unhides the cursor, and use this functionality to execute
  #       find-cursor on unhide.
  # TODO: automatically suspend after a longer time period
  #       automatically hibernate after a still-longer time period
  #       automatically sleep/hibernate after some time (probably hibernate, for encryption/batt power)
  #       Or don't do any of this, because sometimes I leave long-running processes and this would
  #       be really annoying. Or maybe an easy way to toggle these things on/off; i.e. via polybar?
  #       Should do something similar with auto screen lock.
  # TODO: auto-update nix install
  # TODO: git integration for command-line prompt. Show branch (text) and status (with colour? or
  #       as text?).
  # TODO: if possible, change encryption to use: first) yubikey, second) otp, third) password?
  #       https://www.google.com/search?q=luks%20multiple%20options%20for%20decryption
  # TODO: spotify cli with discovery features? Basically a recreation of the spotify ui in cli?
  # TODO: pulseaudio-chromecast
  # TODO: add some nix-specific instructions.. or a readme or something..
  # TODO: incorporate zshrc where appropriate
  # TODO: brightness control. xmonad? setxkbmap?
  # TODO: key binding to toggle touchpad, touchscreen on/off. Or just disable clicking with
  #       touchpad? Allow cursor movement? Is there any point (hur hur)?
  #       xinput disable $(xinput list | grep -i Touchpad | grep -o 'id=[0-9]\+' | grep -o '[0-9]\+')
  #       Is this necessary now that touchpad is disabled while typing?
  # TODO: low battery detection and notification
  # TODO: map caps lock to escape?
  # TODO: put zsh history into sqlite db
  # TODO: change from oh-my-zsh to antigen. Or just nix-managed plugins?
  #       `man home-configuration.nix` has an example of this under programs.zsh.plugins
  # TODO: auto-dim screen, or apply power-saving methods automatically when external power is
  #       removed? And vice-versa?
  # TODO: possible to allow non-root users to mount storage, with non-root rw permissions? Is there
  #       a compromise where I enter frequently used devices UUIDs in fstab? (Is that a good idea
  #       from a security standpoint?)
  # TODO: put vimperator conf in here
  # TODO: put a "hide fixed elements" script+hotkey in vimperator
  # TODO: move to sway/wayland for better multi monitor support?
  # TODO: system-wide dark/light mode- possible?
  #       - alacritty live-reloads config, so easy for terminals
  #       - Firefox and Chrome both have control over themes, and should one way or another be able
  #         to live reload
  #       - CSS: https://news.ycombinator.com/item?id=26348378
  # TODO: incorporate firefox extension configuration files. Configuration appears to be stored in
  #       ~/.mozilla/firefox/<profile>/browser-extension/data
  # TODO: TUI *streaming* JSON log viewer and filter. Use jq (syntax?).
  #       - expect logs to be line-based
  #       - pretty-print preview window for filter results? Or allow user to expand/contract?
  #       - support key chords for control
  #       - support modal control- "insert" mode for searching, "command" mode for navigation
  #       - support ekmett lens syntax?
  #       - support jsonpath syntax?
  #       - support multiple parsers?
  #       - support saving filter results
  #       - should this just be a mode for lnav?
  #       - support parsing and feature identification
  #         - we could, for example, allow the user to provide a config file that identifies a
  #           stack trace in the log output, then allow them to select that stack trace and pass it
  #           to a subshell in a structured manner (JSON!) for deeper inspection. For example, they
  #           could pass it to fzf, then search/navigate up and down it the stack, and view the
  #           selected file/line in the stack trace in their preview window, and open the relevant
  #           file in their editor from fzf. Or each file in the stack could be opened in the
  #           editor in order, focused on the specific line from the stack trace.
  #         - could also identify things like request objects, and produce them as
  #           curl/fetch/httpie etc.
  #         - is there any sense in tree sitter here, just for speed? Or are there other faster
  #           parsers and tree sitter is just good for changes?- we should only need to parse fast
  #           once
  #         - this could function as a "log filter" also, the tool could be run as part of a
  #           pipeline, where it acts as an interactive log filter, passing some output to the next
  #           part of the pipe.
  #       - see also:
  #         - https://github.com/simeji/jid
  #         - https://github.com/dflemstr/rq - for data type support
  #         - https://ilya-sher.org/2018/04/10/list-of-json-tools-for-command-line/
  # TODO: Some version-controlled notes on a per-directory/repo basis, but that aren't actually
  #       stored in the directory/repo (because I don't want to commit them to that place). Then,
  #       when in a local clone of a given repo, I can type a command, e.g. `notes` and have all
  #       relevant notes for that particular repo or directory shown to me. These could be backed
  #       up in a private GH repo. Essentially a plaintext metadata store.
  # TODO: a note _content_ search analogue to `tv`
  # TODO: A k8s admin interface that is more data-oriented than k9s. Specifically: every resource
  #       in k8s can be represented in json, and we have some useful json query/filter languages,
  #       e.g. jq, jsonpath, lenses. We could represent all k8s resources as top-level keys, then
  #       let the user apply a filter of some sort, such that a user could, for example, get all
  #       resources with the label "app.kubernetes.io/managed-by:flux". Could also provide some
  #       shortcut keys for these sorts of things, like <c-l> to add a "like" label filter (with
  #       regex, perhaps?).
  # TODO: use skim for reverse history search- then the interface will be the same everywhere
  # TODO: a date macro that types the date in a user-selected format. Perhaps xmonad? Or new
  #       keyboard..?
  # TODO: TUI HTTP request editor/explorer?
  # TODO: Always show working directory in shell prompt, but truncate if it's too long, rather than
  #       not showing at all. Or perhaps break the line? Or show it on its own line? Could check
  #       out powerline for zsh.
  # TODO: my current vim markdown config kinda sucks. Biggest current objection, when wrapping
  #       lines in a list, it will create a new list item. Can this be avoided? Check
  #       `vim-markdown` plugin- is it just syntax highlighting? Is there something else? Turns out
  #       I write a fair bit of markdown.
  # TODO: spotifyfs. A... wait.. it might already exist! A FUSE interface for Spotify:
  #       https://github.com/catharsis/spotifile
  # TODO: a kubernetes port-forwarder that creates some sort of local shell that controls name
  #       resolution within it. Something like kubefwd that lets you access your k8s services by
  #       their service names, but works without modifying your local /etc/hosts, and allows you to
  #       access your local files without a docker mount or similar.
  #       One idea might be to start a shell within a network namespace where DNS resolution is
  #       controlled to resolve port-forwards, so something like `ip netns exec vnet0 $SHELL` or `ip
  #       netns exec vnet0 su - msk`. E.g.:
  #       - https://serverfault.com/questions/925334/setting-a-custom-etc-hosts-or-resolver-for-one-process-only-in-linux
  #       It might also be possible to use cgroups to achieve this. Some more inspiration:
  #       - https://superuser.com/questions/271915/route-the-traffic-over-specific-interface-for-a-process-in-linux/1048913#1048913
  # TODO: Map keyboard setup to kmonad so other keyboards are a bit less alien. Consider also using
  #       kmonad with keyboard.
  # TODO: kmonad to make keyboard behave the same regardless of machine. I.e. holding z is ctrl.
  # TODO: Tool that just takes k8s manifests, or a k8s manifest list, applies them to the cluster,
  #       and waits until all deployments are ready. This is because kubectl wait is distinct from
  #       kubectl apply, and there is no easy way to wait for everything _that you just applied_.
  #       Perhaps the tool I really want is one that applies, waits for everything that's just been
  #       deployed, and optionally prunes anything that no longer needs to exist. It could perhaps
  #       track resources it "owns" and prune only those. And nothing more.
  # TODO: hotkey to toggle mirrored xmonad layout
  # TODO: libjq? Or a translation layer between lenses and jq? I.e. "traversal systems" as per the
  #       blog post. Then no need for a libjq for languages that have a traversal system; and
  #       either syntax (and potentially other syntaxes; i.e. jsonpath) can be valid for traversal.
  # TODO: Structured editor with which you insert language primitives. I.e. c-b would insert a
  #       block statement (complete with curly braces). C-a would insert an assignment. This could
  #       be simulated with UltiSnips... In fact, it might not need any more than UltiSnips. But
  #       later, it might be desirable for the editor to have text objects that are parts of the
  #       language structure. This might already exist with vim tree sitter implementation. We
  #       might be able to modify hop.nvim to integrate with tree sitter and jump straight to a
  #       (leaf?) node. And similarly with text objects: "change within this node", "comment this
  #       node". Cool... look deeper.
  # TODO: vim: hop/easymotion to textobject; specifically, treesitter text objects; so, jump to
  #       program symbol, i.e. variables, keywords, etc;
  # TODO: Cursor jump to monitor/window
  # TODO: "Toggle to last workspace" XMonad. Like <c-^> in Vim.
  # TODO: giphy roulette Signal plugin
  # TODO: subsume messaging into Matrix bridges?
  # TODO: use systemd-tmpfiles for managing temp files? See systemd.user.tmpfiles.rules in `man
  #       home-configuration.nix`
  # TODO: consider making many of my services (e.g. Firefox) wait for
  #       `systemd-networkd-wait-online.service` as well as `graphical-session-pre.target`.
  # TODO: spell-check in vim; especially for markdown. I wonder if it's possible to spell-check
  #       word boundaries in code. I.e. camelCase or kebab-case etc. Also markdown etc.
  # TODO: make dotfiles literate?
  # TODO: eliminate all shitty web apps, including "chromium apps" by using native calendar, email,
  #       notes clients etc. And perhaps matrix bridge to any messaging services.
  # TODO: single-tab (or untabbed) browser instance with no user data directory for use as
  #       $BROWSER ? Can we bundle webextensions? Perhaps with no user chrome or something?
  # TODO: Per-directory commands and discovery. For example, "build" could be a command for a
  #       project directory. Moreover, "build" could take parameters, such as --debug, or
  #       --release. But how do I know when commands are available, what they are, and what
  #       parameters are available? Availability of commands could perhaps be displayed in my shell
  #       prompt, then a basic help command could display available commands and parameters. This
  #       might be largely achievable with direnv.
  # TODO: password generation with parameters- if this doesn't exist already, make it; TUI/GUI?
  # TODO: kmonad to make keyboard behave the same regardless of machine. I.e. holding z is ctrl.
  # TODO: move ~/.dotfiles to ~/projects/github.com/partiallyordered/dotfiles so it shows up in the
  #       projects list.
}

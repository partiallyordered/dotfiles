{ config, pkgs, lib, ... }:
let
  # Originally from: https://github.com/nix-community/nur-combined/blob/e745144e9650d083bde1c454d4653ba7cdeb9518/repos/rycee/pkgs/firefox-addons/default.nix
  buildFirefoxXpiAddon = { pname, version, addonId, url, sha256, ... }:
    pkgs.stdenv.mkDerivation {
      name = "${pname}-${version}";

      src = builtins.fetchurl { inherit url sha256; };

      preferLocalBuild = true;
      allowSubstitutes = false;

      buildCommand = ''
          dst="$out/share/mozilla/extensions/{ec8030f7-c20a-464f-9b0e-13a3a9e97384}"
          mkdir -p "$dst"
          install -v -m644 "$src" "$dst/${addonId}.xpi"
      '';
    };

  # Some examples from: https://github.com/nix-community/nur-combined/blob/master/repos/rycee/pkgs/firefox-addons/generated-firefox-addons.nix
  myFirefoxAddons = {
    # TODO: https://addons.mozilla.org/en-US/firefox/addon/har-json-viewer/
    # TODO: https://addons.mozilla.org/en-US/firefox/addon/tabcenter-reborn/
    notifier-for-github = buildFirefoxXpiAddon {
      pname = "notifier-for-github";
      version = "20.9.10";
      # To find addonId you need to find the manifest.json of the addon- this might be available in the
      # source code, e.g. https://github.com/dessant/search-by-image/blob/37e905336bb420e72724bef6d71c5aa7b2147723/src/manifest/firefox.json
      # It might also be possible to download the .xpi file (just a .zip file) at $url below,
      # extract it, and examine the manifest.
      # It seems to be possible for an extension to lack an id. See save-to-wayback-machine below.
      # In this case, it seems as though using any id works, but it may be necessary to
      # subsequently browse to the add-on in the firefox add-ons store and install the add-on
      # manually. Additionally, this shouldn't *really* matter, because we have the checksum- so
      # the ID is kind of irrelevant to us. (Use the checksum as the ID?).
      addonId = "{8d1582b2-ff2a-42e0-ba40-42f4ebfe921b}";
      # url is the URL that the [+ Add to Firefox] button on the add-on page will send you to
      url = "https://addons.mozilla.org/firefox/downloads/file/3640918/notifier_for_github-20.9.10-an+fx.xpi";
      # nix-prefetch-url $url
      # where $url is the url from above
      sha256 = "12dv7wqqvsg1chr1k0kcqsdwyca2qgm8lk6x1dxcvv63sb4wk005";
    };
    loadtabonselect3 = buildFirefoxXpiAddon {
      pname = "loadtabonselect3";
      version = "1.2022.223.0";
      addonId = "{85d2532b-a793-4048-8ef4-713af1ff320d}";
      url = "https://addons.mozilla.org/firefox/downloads/file/3913188/loadtabonselect_3-1.2022.223.0-fx.xpi";
      sha256 = "1lxz3857mypa9nbdxgbq8h1bf3wx8pvl3l8glz4d1kb44a3fvsmh";
    };
    hide-fixed-elements = buildFirefoxXpiAddon {
      pname = "hide-fixed-elements";
      version = "1.2";
      addonId = "hidefixedelements@commonground.systems";
      url = "https://addons.mozilla.org/firefox/downloads/file/832286/hide_fixed_elements-1.2-an+fx.xpi";
      sha256 = "05mzmddd9kql64kn29vp8f2kznfzc4fjp4qz7m88syi9snxkkdjg";
    };
    redirector = buildFirefoxXpiAddon {
      pname = "redirector";
      version = "3.5.3";
      addonId = "redirector@einaregilsson.com";
      url = "https://addons.mozilla.org/firefox/downloads/file/3535009/redirector-3.5.3-an+fx.xpi";
      sha256 = "0w8g3kkr0hdnm8hxnhkgxpf0430frzlxkdpcsq5qsx2fjkax7nzd";
    };
    onetab = buildFirefoxXpiAddon {
      pname = "onetab";
      version = "1.54";
      addonId = "extension@one-tab.com";
      url = "https://addons.mozilla.org/firefox/downloads/file/3739030/onetab-1.54-an+fx.xpi";
      sha256 = "07awjghwrfc1l199rxh7adww25piggy3s53ash0p06hbmdygc8ni";
    };
    skip-redirect = buildFirefoxXpiAddon {
      pname = "skip-redirect";
      version = "2.3.4";
      addonId = "skipredirect@sblask";
      url = "https://addons.mozilla.org/firefox/downloads/file/3632211/skip_redirect-2.3.4-an+fx.xpi";
      sha256 = "0fhv5xjp02fviaw4ai7bjmfjjg1vbfhn5v9038ra3b0hckm39r5y";
    };
    to-google-translate = buildFirefoxXpiAddon {
      pname = "to-google-translate";
      version = "4.2.0";
      addonId = "jid1-93WyvpgvxzGATw@jetpack";
      url = "https://addons.mozilla.org/firefox/downloads/file/3798719/to_google_translate-4.2.0-fx.xpi";
      sha256 = "1mpjcpq4ybfpgqmvf3cp5hkpym3v42hc47lkdvkh3q8gsnrj4fqv";
    };
    transover = buildFirefoxXpiAddon {
      pname = "transover";
      version = "1.63";
      addonId = "transover";
      url = "https://addons.mozilla.org/firefox/downloads/file/3901898/transover-1.63-an+fx.xpi";
      sha256 = "13zydnhqyxwhzgmqvl8fb4vcn33vyrvhaniyhvq8g9jbigayg4m9";
    };
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
    in
      s // {
        Service = s.Service // {
          CPUQuota = cpu;
          MemoryMax = mem;
        };
      };

  chromiumApp = { name, desc, url, env ? "", profile ? name }:
    # TODO: is there a way to "deep-replace" in the output of
    # constrainedService instead of having the awkward .Service replacement
    # below?
    let s = constrainedService {
      cpu = "150%";
      mem = "2G";
      desc = desc;
      cmd = "${pkgs.chromium}/bin/chromium --app=https://${url} --class=${name} --user-data-dir=\$HOME/.config/chromium_${profile} --force-dark-mode";
      env = env;
    };
    in
      s //
        {
          Service = s.Service // {
            # need to escape nix and systemd unit file syntax
            ExecStartPre = "/run/current-system/sw/bin/mkdir -p \${HOME}/.config/chromium_${name}";
          };
        };

  customVimPlugins = {
    # override builtin hop-nvim due to bug
    hop-nvim = pkgs.vimUtils.buildVimPlugin {
      name = "hop.nvim";
      src = pkgs.fetchFromGitHub {
        owner = "phaazon";
        repo = "hop.nvim";
        rev = "e2f978b50c2bd9ae2c6a4ebdf2222c0f299c85c3";
        sha256 = "1si2ibxidjn0l565vhr77949s16yjv46alq145b19h15amwgq3g2";
      };
    };
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
    # Plugin 'https://github.com/Raimondi/delimitMate/' # using auto-pairs now, is it better?
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
    # plugins = [ "git" "sudo" "cabal" "docker" "npm" "systemd" "vi-mode" ];
  ];

in
{
  programs.direnv.enable = true;
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
  programs.firefox = {
    enable = true;
    # list here:
    # https://github.com/nix-community/nur-combined/blob/master/repos/rycee/pkgs/firefox-addons/addons.json
    extensions = with pkgs.nur.repos.rycee.firefox-addons; with myFirefoxAddons; [
      browserpass
      darkreader
      decentraleyes
      https-everywhere
      link-cleaner
      old-reddit-redirect
      private-relay
      react-devtools
      search-by-image
      tridactyl
      ublock-origin
      wayback-machine

      loadtabonselect3
      hide-fixed-elements
      notifier-for-github
      redirector
      skip-redirect
      to-google-translate
      transover
      onetab
    ];
    profiles = {
      default = {
        id = 0;
        settings = {
          "browser.search.region" = "GB";
          "browser.search.isUS" = false;
          "distribution.searchplugins.defaultLocale" = "en-GB";
          "general.useragent.locale" = "en-GB";
          "browser.bookmarks.showMobileBookmarks" = true;
          # https://old.reddit.com/r/firefox/comments/fyqrd7/new_tab_in_dark_mode/fn1mt4f/
          # https://gist.github.com/gmolveau/a802ded1320a7591a289fb7abd0d6c45
          "toolkit.legacyUserProfileCustomizations.stylesheets" = true;
        };
        # https://old.reddit.com/r/firefox/comments/fyqrd7/new_tab_in_dark_mode/fn1mt4f/
        # https://gist.github.com/gmolveau/a802ded1320a7591a289fb7abd0d6c45
        userChrome = ''
          tabbrowser tabpanels { background-color: rgb(19,19,20) !important; }
          browser { background-color: rgb(19,19,20) !important; }
          /* Replace the white flash before a page loads */
          :root {
            --in-content-page-background: rgb(19,19,20) /*flash on new tab*/
          }
          /* Color of pre-load content area */
          #browser vbox#appcontent tabbrowser,
          #content, #tabbrowser-tabpanels,
          browser[type=content-primary],
          browser[type=content] > html {
            background: var(--in-content-page-background) !important
          }
          /* Number tabs */
          tab {
              counter-increment: tab-number;
          }
          .tab-text:before {
              content: counter(tab-number) ": ";
          }
        '';
        userContent = ''
          /* dark "unable to connect", "dns not found", other error pages */
          body.illustrated.neterror {
            background-color: rgb(19,19,20) !important;
            color: #a8a8a8 !important;
          }
          body.illustrated.neterror div.title h1 {
            color: #a8a8a8 !important;
          }
          /* dark new tab */
          @-moz-document
          url("about:blank"),
          url("about:home"),
          url("about:newtab") {
            body {
              background: rgb(19,19,20) !important;
            }
            html > body:empty {
              background-color: rgb(19,19,20) !important;
            }
          }
        '';
      };

    };
  };

  # TODO: should this be done with xdgConfigDirs (or whatever it's called)?
  home.file = {
    yamllint = {
      source = ./yamllint/config.yaml;
      target = ".config/yamllint/config";
    };
    sackrc = {
      source = ./.sackrc;
      target = ".sackrc";
    };
    alacrittyConf = {
      source = ./alacritty.yml;
      target = ".config/alacritty/alacritty.yml";
    };
    ultisnipsKubernetesSnippets = {
      source = ./ultisnips;
      target = ".config/nvim/UltiSnips";
    };
  };

  home.keyboard.layout = "gb";
  # home.{language,currency,time,etc.}- see `man home-configuration.nix`

  xsession = {
    enable = true;
    windowManager.xmonad = {
      enable = true;
      enableContribAndExtras = true;
      config = ./xmonad.hs;
    };
    pointerCursor = {
      # TODO: but, but I just want to change the pointer size. Why do I have to
      # have this other stuff? Is there a default somewhere that I can override?
      size = 128;
      name = "Vanilla-DMZ";
      package = pkgs.vanilla-dmz;
    };
  };

  programs.autorandr = {
    # DPI of Dell U2718Q: 163
    # From:
    # https://www.displayspecifications.com/en/model/fed0d61
    enable = true;
    profiles = {
      # TODO: read more here: https://github.com/rycee/home-manager/blob/master/modules/programs/autorandr.nix
      van = {
        fingerprint = {
          "DP-1" = "00ffffffffffff0030aedd6100000000271e0104a51f12783aee95a3544c99260f5054bdcf84a94081008180818c9500950fa94ab300023a801871382d40582c450035ae1000001e000000fc004d31340a202020202020202020000000fd00324b1e5a14000a202020202020000000ff00563930364c464e320affffffff0100020316b14b9005040302011f1213141165030c0010007c2e90a0601a1e4030203600dc0b1100001cab22a0a050841a3030203600dc0b1100001c662156aa51001e30468f3300dc0b1100001e00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000008";
          "eDP-1" = "00ffffffffffff004d108d1400000000051c0104a52213780ed920a95335bc250c5155000000010101010101010101010101010101014dd000a0f0703e803020350058c210000018000000000000000000000000000000000000000000fe00464e564452804c513135364431000000000002410328011200000b010a20200090";
        };
        config = {
          "DP-1" = {
            enable = true;
            primary = true;
            position = "0x0";
            dpi = 158;
            mode = "1920x1080";
            rate = "60.00";
            transform = [
              [ 1.5 0.0 0.0 ]
              [ 0.0 1.5 0.0 ]
              [ 0.0 0.0 1.0 ]
            ];
            # TODO: gamma? see `man home-configuration` and `xrandr --props`
            # TODO: put a scale-from command in the postswitch script?
          };
        };
      };
      undocked = {
        fingerprint = {
          "eDP-1" = "00ffffffffffff004d108d1400000000051c0104a52213780ed920a95335bc250c5155000000010101010101010101010101010101014dd000a0f0703e803020350058c210000018000000000000000000000000000000000000000000fe00464e564452804c513135364431000000000002410328011200000b010a20200090";
        };
        config = {
          "eDP-1" = {
            enable = true;
            primary = true;
            position = "0x0";
            mode = "3840x2160";
            rate = "60.00";
            # From https://topics-cdn.dell.com/pdf/xps-15-9570-laptop_specifications_en-us.pdf
            # Width:  344.21 mm (13.55 in)
            # Height: 193.62 mm (7.62 in)
            # Resolution: 3840x2160
            # DPI should be 283
            dpi = 283;
            transform = [
              [ 1.5 0.0 0.0 ]
              [ 0.0 1.5 0.0 ]
              [ 0.0 0.0 1.0 ]
            ];
            # TODO: gamma? see `man home-configuration` and `xrandr --props`
            # TODO: put a scale-from command in the postswitch script?
          };
        };
      };
    };
    hooks = {
      postswitch = {
        # Restarting XMonad seems to cause windows to redraw. This is good, because otherwise some
        # windows secretly still believe they're whatever size they were before the restart, and
        # mouse interaction with them is strange.
        "restart-xmonad" = "${config.xsession.windowManager.command} --restart";
    #     "change-dpi" = ''
    #       case "$AUTORANDR_CURRENT_PROFILE" in
    #         # From https://topics-cdn.dell.com/pdf/xps-15-9570-laptop_specifications_en-us.pdf
    #         # Width:  344.21 mm (13.55 in)
    #         # Height: 193.62 mm (7.62 in)
    #         # Resolution: 3840x2160
    #         # DPI should be 283, but this is ugly
    #         undocked)
    #           DPI=283
    #           ;;
    #         van)
    #           DPI=163
    #           ;;
    #         *)
    #           echo "Unknown profile: $AUTORANDR_CURRENT_PROFILE. Could not set DPI."
    #           exit 1
    #         esac
    #
    #         echo "Xft.dpi: $DPI" | ${pkgs.xorg.xrdb}/bin/xrdb -merge
    #     '';
      };
    };
  };

  programs.broot = {
    enable = true;
    enableZshIntegration = true;
    verbs = [
      {
        key = "ctrl-u";
        internal = ":input_clear";
      }
      {
        key = "ctrl-w";
        internal = ":input_del_word_left";
      }
      {
        key = "ctrl-p";
        internal = ":toggle_preview";
      }
      {
        key = "ctrl-h";
        internal = ":toggle_hidden";
      }
      {
        invocation = "edit";
        key = "enter";
        external = "${pkgs.neovim}/bin/nvim {file}";
        leave_broot = false;
        apply_to = "file";
      }
    ];
  };

  programs.password-store = {
    enable = true;
    package = pkgs.pass.withExtensions (exts: [exts.pass-otp]);
  };
  services.password-store-sync.enable = true;

  programs.git = {
    enable = true;
    userEmail = "mattkingston@gmail.com";
    userName = "msk-";
    extraConfig = {
      # Useful for extraConfig: https://git-scm.com/book/en/v2/Customizing-Git-Git-Configuration
      merge.tool = "vimdiff";
      mergetool.prompt = "true";
      # TODO: get rid of one of $LOCAL $REMOTE $MERGED? Don't really want three-way split. Can we
      # just use vimdiff2? Or is it better to use opendiff, kdiff or something else for merges?
      "mergetool \"vimdiff\"".cmd = "nvim -d $LOCAL $REMOTE $MERGED -c '$wincmd w' -c 'wincmd J'";
      difftool.prompt = "false";
      diff.tool = "vimdiff2";
      diff.algorithm = "histogram";
      url = { "ssh://git@github.com" = { insteadOf = "https://github.com"; } ; } ;
      url = { "ssh://git@gitlab.modusbox.io" = { insteadOf = "https://gitlab.modusbox.io"; } ; } ;
      color.ui = "true";
      pull.rebase = "false";
      credential.helper = "libsecret";
    };
  };

  programs.keychain = {
    enable = true;
    enableZshIntegration = true;
    keys = [ "id_ecdsa" ];
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
    shellAliases =
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
        # exploration etc.?
        b64 = "${pkgs.coreutils}/bin/base64";
        b64d = "${pkgs.coreutils}/bin/base64 --decode";
        chown = "chown -h";
        fi = "${pkgs.fd}/bin/fd";
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
        grohm = "${git} stash push -m \"reset $(${date} -u -Iseconds)\" && ${git} reset --hard origin/master";
        gst = "${git} status";
        gsti = "${git} status --ignored";
        gsw = "${git} switch";
        hms = "${pkgs.home-manager}/bin/home-manager switch";
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
        # TODO: can we make this a global alias?
        pg = "| grep";
        refcp = "${git} rev-parse HEAD | tr -d '\n' | ${xclip} -i -sel clipboard -f | ${xclip} -i -sel primary -f";
        scf = "${systemctl} --state=failed";
        sc = "${systemctl}";
        scratch = "cd ~/projects/scratch";
        scur = "${systemctl} --user restart";
        scus = "${systemctl} --user status";
        scu = "${systemctl} --user";
        ssh = "${pkgs.mosh}/bin/mosh --predict=experimental";
        stripcolours="sed -r 's/\\x1B\\[([0-9]{1,2}(;[0-9]{1,2})?)?[mGK]//g'";
        tree = "${exa} --all -T --git-ignore -I.git";
        ts = ''
          ${sk} \
            --delimiter ':' \
            --ansi \
            -i \
            -c '${rg} -n --ignore-vcs --color=always "{}"' \
            --preview '${bat} --style=numbers,changes --color=always -r "$(${calc} -p "floor(max(1, $(${expr} {2}) - $LINES / 2))"):$(${calc} -p "floor($LINES + max(0, $(${expr} {2}) - $LINES / 2))")" -H{2} {1}'
          '';
        # TODO: the following, but with a language server generating the input list i.e. tokens?
        # Perhaps look at https://github.com/lotabout/skim.vim
        # TODO: would be nice to add a search term to nvim startup, e.g. `nvim {1} +{2} +/{0}`. At
        # the time of writing, skim doesn't supply the current search term to the executed program,
        # AFAICT.
        # TODO: making this a shell function would let us take an optional argument to the --query
        # parameter, so we could use `vs "some text to search"`. The advantage of this would be
        # that this query would go into the shell command history.
        vs = ''
          ${sk} \
            --bind "enter:execute(${nvim} {1} +{2})" \
            --delimiter ':' \
            --ansi \
            -i \
            -c '${rg} -n --ignore-vcs --color=always "{}"' \
            --preview '${bat} --style=numbers,changes --color=always -r "$(${calc} -p "floor(max(1, $(${expr} {2}) - $LINES / 2))"):$(${calc} -p "floor($LINES + max(0, $(${expr} {2}) - $LINES / 2))")" -H{2} {1}'
        '';
        vd = "${nvim} -d";
        v = "${nvim}";
        weather = "${pkgs.curl}/bin/curl http://v2.wttr.in";
      };
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
      auto-pairs
      awesome-vim-colorschemes
      cmp-nvim-lsp
      cmp_luasnip
      dart-vim-plugin
      easy-align
      editorconfig-vim
      fugitive
      haskell-vim
      hop-nvim
      lualine-nvim
      luasnip
      markdown-preview-nvim
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
      vim-flutter
      vim-go
      vim-gh-line
      vim-hcl
      vim-indent-object
      vim-javascript
      vim-markdown
      vim-nix
      vim-terraform
      vim-toml
      vim-yaml-folds
      zig-vim
    ];
  };

  home.sessionVariables = {
    EDITOR = "vim";
    # TODO: this chromium instance has its data dir created at $BROWSER variable creation time, not
    # call time. Might need a wrapper script.
    # BROWSER = "chromium --incognito --user-data-dir=$(mktemp -d)";
    BROWSER = "${pkgs.firefox}/bin/firefox --private-window";
    TERMCMD = "${pkgs.alacritty}/bin/alacritty";
  };

  # TODO: auto-restart services?
  systemd.user.services.firefox = basicService {
    desc = "Firefox";
    cmd = "${pkgs.firefox}/bin/firefox";
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
  systemd.user.services.mullvad = {
    Unit = {
      Description = "Mullvad GUI";
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
  systemd.user.services.whatsapp = chromiumApp
    { name = "whatsapp"; desc = "WhatsApp Web"; url= "web.whatsapp.com"; };
  systemd.user.services.calendar = chromiumApp
    { name = "calendar"; desc = "Calendar"; url = "calendar.google.com"; profile = "google"; };
  systemd.user.services.gmail = chromiumApp
    { name = "gmail"; desc = "Gmail"; url = "mail.google.com"; profile = "google"; };
  systemd.user.services.fbmessenger = chromiumApp
    { name = "messenger"; desc = "Facebook Messenger"; url = "messenger.com"; };
  systemd.user.services.signal = constrainedService
    { desc = "Signal"; cmd = "${pkgs.signal-desktop}/bin/signal-desktop"; };
  systemd.user.services.spotify = constrainedService
    { desc = "Spotify"; cmd = "${pkgs.spotifywm}/bin/spotifywm"; };
  systemd.user.startServices = false;
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

  home.packages = with pkgs; [
    alacritty
    android-studio
    arandr
    ascii
    authy
    # bingo
    # binutils-unwrapped
    bat
    blueman
    cabal2nix
    calc
    cargo
    cargo-edit
    dmenu
    dnsutils
    docker-compose
    doctl
    dos2unix
    entr
    exa
    exfat
    expect
    fd
    ffmpeg
    flutter
    fzy
    gcc
    gh
    ghc
    git
    gitAndTools.hub
    gnumake
    gnumeric
    gnupg
    go
    # Check whether golang's official lang server implementation is available yet. Or perhaps use
    # this, per the advice on the gh page for the sourcegraph lang server implementation:
    # https://github.com/saibing/bingo. See the derivation earlier in this file for bingo.
    go-langserver
    graphviz
    haskell-language-server
    jid
    jq
    k3s
    keybase-gui
    kubeconform
    kubectl
    kustomize
    lazygit
    ldns # drill
    libnotify
    libreoffice
    libsecret
    lnav
    marble
    mcfly
    moreutils
    mosh
    mullvad-vpn
    mycli
    mysql
    # TODO: cannot build at the time of writing as there is a python interpreter compatibility
    # issue:
    #   > error: bcrypt-3.2.0 not supported for interpreter python2.7
    #   > (use '--show-trace' to show detailed location information)
    # issue tracked here (at least): https://github.com/NixOS/nixpkgs/issues/97642
    # mysql-workbench
    myNode
    ncpamixer
    nmap
    nodePackages.typescript-language-server
    nodePackages.node2nix
    oathToolkit
    openssh
    openssl
    pciutils
    # At the time of writing, unused, and causing a build failure
    # platformio
    pueue
    python37Packages.sqlparse
    pwgen
    ripgrep
    rnix-lsp
    rust-analyzer
    rustc
    signal-desktop
    skaffold
    skim
    silver-searcher
    socat
    spotify-tui
    stack
    inetutils
    texlive.combined.scheme-small # pdflatex for pandoc pdf output
    transmission # TODO: transmission service?
    tree
    tree-sitter
    unzip
    usbutils
    vlc
    wireguard-tools
    wireshark
    xclip
    xorg.xdpyinfo
    xsel
    xxd
    yaml-language-server
    yamllint
    yarn
    youtube-dl
    yj
    yq
    zeal
    zig
    zip
    zls
    zoom-us

    dejavu_fonts
    inconsolata
    liberation_ttf
    powerline-fonts
    terminus_font
    ttf_bitstream_vera
    # vistafonts # marked as broken
  ];

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
      };
    };
  };

  services.redshift = {
    enable = true;
    # Paris
    latitude = "48.8566";
    longitude = "2.3522";
  };

  # services.gnome-keyring = {
  #   enable = true;
  #   components = [ "pkcs11" "secrets" "ssh" ];
  # };
  programs.gpg.enable = true;
  services.gpg-agent = {
    pinentryFlavor = "gtk2";
    enable = true;
    enableSshSupport = true;
  };

  # TODO: turn the screen off immediately after we lock it
  services.screen-locker = {
    enable = true;
    inactiveInterval = 5;
    lockCmd = "sudo systemctl start physlock";
  };

  services.polybar = {
    enable = true;
    package = pkgs.polybar.override { pulseSupport = true; mpdSupport = true; githubSupport = true; };
    config = ./polybar/space.ini;
    script = "polybar top &";
  };

  xdg = {
    enable = true;
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
  # http://chriswarbo.net/projects/nixos/useful_hacks.html
  # Check config for other zsh modules
  # https://dougblack.io/words/zsh-vi-mode.html
  # https://terminalsare.sexy/
  # Check config for various vim plugins

  # TODO: translations for Firefox with hover results, preferably using a local translation model
  #       (although presumably if this was easy there wouldn't be an EU-funded Firefox translation
  #       project)
  # TODO: ergonomic password/passphrase generator. Yes, this will reduce entropy, but in exchange
  #       the user could have a longer password/passphrase that prioritises alternating hands on
  #       the keyboard, for example, or words that are easier to type (don't require the pinky,
  #       etc.).
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
  # TODO: set users up with preset password:
  #       https://nixos.org/manual/nixos/stable/options.html#opt-users.users._name_.initialHashedPassword
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
  # TODO: grobi
  # TODO: wrap chromium with wrapProgram to enforce incognito, and set GDK_DPI_SCALE?
  #       | https://nixos.wiki/wiki/Nix_Cookbook#Wrapping_packages
  # TODO: auto-suspend at a certain battery level
  # TODO: figure out how to turn off all radios etc. for flight mode
  # TODO: tridactyl
  #       | replace Hide Fixed Elements browser extension with a Tridactyl script; inspect the Hide Fixed Elements code to see if it does anything other than add a css rule)
  #       | option to grayscale page when following hint. I.e. pressing f temporarily grays the page. This would mean using nicer colours for hints would be more feasible, as they wouldn't clash with pages.
  #       | hide fixed elements
  #       | enter/exit reader mode
  #       | tridactylrc
  #       | guiset/userChrome.css to control the chrome
  # TODO: make an easy key combo (comparable to <M-Return> for terminal) for opening a disposable
  #       chromium (with vimium? Or a disposable nyxt?)
  # TODO: tv
  #       | make a fancier `tv` to show a preview, if it exists? (use bat)
  #       | allow deletion from the prompt?
  #       | provide option to cat the contents of a note to terminal instead of editing?
  #       | does not support path separator (forward-slash) in note names- replace automatically? error? support?
  #       | note content search?
  #       | namespacing? with directories? or is that better handled in the filename (with forward-slashes, even?)?
  #       | force creation of a new note: if I have a note called 'abc' and I want to create a note called 'ab' the current functionality does not allow this (try it)
  #       | allow command-line arguments as a seed to the search? advantage of this is that they'll go into shell command history
  #       | full-text search, perhaps with a different command?
  #       | use `br` as part of tv?
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
  # TODO: put firefox (work and personal) into systemd service?
  # TODO: in status bar | indicator for internet connection status (TCP connection status? DNS,
  #                     |   aggregate connectivity to various services; i.e. GH, messaging, email).
  #                     |   systemd-networkd-wait-online.service might be useful here too
  #                     |   see output of networkctl status; could use something from there
  #                     | DNS resolution status (i.e. can I resolve DNS right now?)
  #                     | pueue status (pueue can push updates, and produce status as json)
  #                     | expected battery life, usage rate?
  #                     | is the nvidia gpu on? # echo '\_SB.PCI0.PEG0.PEGP._OFF' > /proc/acpi/call
  #                     | screen brightness
  #                     | connected vpn name
  #                     | poll "Am I Mullvad"?
  #                     | whether the system is in a degraded state (systemctl status, systemctl --user status)
  #                     | is there some way to characterise internet connectivity without abusing it?
  #                     | which wifi network am I connected to? (is that already in current polybar config?)
  #                     | wifi network signal strength + speed (see `nmcli device wifi list`)
  #                     | status of dotfile directory? status of working git repos? (did I forget to check something in?)
  #                     | caps/num-lock?
  #                     | touchpad on/off status/toggle?
  #                     | touchscreen on/off status/toggle?
  #                     | remaining battery life or time-until-charged
  #                     | charging/discharging state
  #                     | systemctl --user status xautolock AND hotkey/button to enable/disable xautolock
  #                     | use kde connect to show phone battery/notifications?
  #                     | connected devices (bluetooth)
  #                     | menu to select autorandr config
  #                     | input (microphone) and output volume control - perhaps with dropdown?
  #                     | clicking date/time jumps to calendar?
  #                     | high power usage warning?
  #                     | am I running an old kernel? (i.e. do I need a restart?)
  #                     | current up/down data rate
  #                     | time in multiple time-zones (UTC? my team?)
  #                     | screenshot- perhaps a button for an instant screenshot, and one for a two second delay then screenshot
  #                     | CPU/mem usage & CPU temp?
  #                     | GH notifications
  #                     | WhatsApp, Keybase, FB Messenger, Slack, gmail notifications?
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
  # TODO: programs.rofi.enable; # consider, but it looks pretty heavy-weight..
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
  # TODO: automatically lock screen after some time period
  # TODO: automatically suspend after a longer time period
  # TODO: automatically hibernate after a still-longer time period
  # TODO: auto-update nix install
  # TODO: git integration for command-line prompt. Show branch (text) and status (with colour? or
  #       as text?).
  # TODO: ligatures, especially for haskell
  #       https://github.com/tonsky/FiraCode
  #       https://www.google.com/search?q=vim%20haskell%20fira%20code
  #       https://www.hanselman.com/blog/MonospacedProgrammingFontsWithLigatures.aspx
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
  # TODO: automatically sleep/hibernate after some time (probably hibernate, for encryption/batt power)
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
  # TODO: .ignore file is not placed appropriately
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
}

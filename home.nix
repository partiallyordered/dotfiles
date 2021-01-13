{ config, pkgs, lib, ... }:
let
  nur = import (builtins.fetchTarball "https://github.com/nix-community/NUR/archive/master.tar.gz") {
    inherit pkgs;
  };

  filesIn = with lib; with builtins; dir: suffix:
    foldl
      (a: b: a + "\n" + b)
      ""
      (attrValues
          (mapAttrs
            (name: _: readFile (dir + "/${name}"))
            (filterAttrs (name: type: hasSuffix ".${suffix}" name && type == "regular") (readDir dir))));

  # Copied from https://github.com/NixOS/nixpkgs/blob/master/pkgs/development/tools/kustomize/default.nix
  kustomize391 = pkgs.buildGoModule rec {
    pname = "kustomize";
    version = "3.9.1";
    # rev is the 3.9.1 commit, mainly for kustomize version command output
    rev = "826b5d9792fb67c4d8f8cd59747698ebf0b22720";

    buildFlagsArray = let t = "sigs.k8s.io/kustomize/api/provenance"; in
      ''
        -ldflags=
          -s -X ${t}.version=${version}
          -X ${t}.gitCommit=${rev}
      '';

      src = pkgs.fetchFromGitHub {
        owner = "kubernetes-sigs";
        repo = pname;
        rev = "kustomize/v${version}";
        sha256 = "1v8yfiwzg84bpdh3k3h5v2smxx0dymq717r2mh3pjz3nifkg3ilm";
      };

    # avoid finding test and development commands
    sourceRoot = "source/kustomize";

    vendorSha256 = "1nixkmyqzq7387rwam0bsa6qjd40k5p15npq0iz1z2k1ws8pvrg6";

    meta = with lib; {
      description = "Customization of kubernetes YAML configurations";
      longDescription = ''
        kustomize lets you customize raw, template-free YAML files for
        multiple purposes, leaving the original YAML untouched and usable
        as is.
      '';
      homepage = "https://github.com/kubernetes-sigs/kustomize";
      license = licenses.asl20;
      maintainers = with maintainers; [ carlosdagos vdemeester periklis zaninime ];
    };
  };

  # To add to this, add packages of interest to node-packages.json, then run
  # `node2nix -10 -i node-packages.json`
  # `home-manager switch`
  myNode = pkgs.nodejs-14_x;
  myNodePackages = import ./default.nix {
    nodejs = myNode;
  };

  basicService = { desc, cmd, env ? "" }:
    {
        Unit = {
          Description = desc;
          After = [ "graphical-session-pre.target" ];
          PartOf = [ "graphical-session.target" ];
        };

        Install = {
          WantedBy = [ "graphical-session.target" ];
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

  chromiumApp = { name, desc, url, env ? "" }:
    # TODO: is there a way to "deep-replace" in the output of
    # constrainedService instead of having the awkward .Service replacement
    # below?
    let s = constrainedService {
      cpu = "150%";
      mem = "2G";
      desc = desc;
      cmd = "${pkgs.chromium}/bin/chromium --app=https://${url} --class=${name} --user-data-dir=\$HOME/.config/chromium_${name} --force-dark-mode";
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
    markdown-preview = let
      # # Requires newer node2nix. 1.8.0 does not support .nodeDependencies
      # deps = (pkgs.callPackage ./markdown-preview {}).nodeDependencies;
    in pkgs.vimUtils.buildVimPluginFrom2Nix {
      pname = "markdown-preview";
      version = "c6bf4412b247ef09fab2adf3bc6f1056d0c57a4f";
      src = pkgs.fetchFromGitHub {
        owner = "iamcco";
        repo = "markdown-preview.nvim";
        rev = "c6bf4412b247ef09fab2adf3bc6f1056d0c57a4f";
        sha256 = "0jgzvmpmzsjgcwdpf861qd57hqg9qh5zvhys4bwjj9f8nqzlmz3v";
      };
      # buildInputs = [ myNode deps ];
      # postInstall = ''
      #   cd app
      #   ln -s ${deps}/lib/node_modules ./node_modules
      #   npm install
      # '';
    };
    vim-gh-line = pkgs.vimUtils.buildVimPlugin {
      name = "vim-gh-line";
      src = pkgs.fetchFromGitHub {
        owner = "ruanyl";
        repo = "vim-gh-line";
        rev = "119fd11a6d504e9c672b6361338fe1382de9399d";
        sha256 = "07cq4fgas2cg7ypy4h70mdqny6mqxhf3ylbxlnybbzk3lz2kwzmc";
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
    {
      name = "zsh-fzy";
      src = pkgs.fetchFromGitHub {
        owner = "aperezdc";
        repo = "zsh-fzy";
        rev = "5d54f3927529b8d8a105376a3b51e51bb3fa3ca2";
        sha256 = "1yncmcsyz4ch9i57cvix1hsl9915r7sj0vffbx1q3dsv9n6x3wgn";
      };
    }
    {
      name = "enhancd";
      file = "init.sh";
      src = pkgs.fetchFromGitHub {
        owner = "b4b4r07";
        repo = "enhancd";
        rev = "ef0dd7d3dda10529d7fe17500ee5550e72dda19c";
        sha256 = "1h7q0qnzz4jn0yav8b67kj7jfvy7ws4jvx9k7w9ck6ynxp98qszx";
      };
    }
    # plugins = [ "git" "sudo" "cabal" "docker" "npm" "systemd" "vi-mode" ];
  ];

in
{
  programs.direnv.enable = true;
  programs.home-manager.enable = true;
  programs.home-manager.path = https://github.com/rycee/home-manager/archive/master.tar.gz;
  programs.feh.enable = true;
  programs.htop.enable = true; # TODO: check out the config options you didn't know were there
  programs.chromium = {
    enable = true;
    extensions = [
      # TODO: privacy badger?
      # TODO: videostream?
      "fmkadmapgofadopljbjfkapdkoienihi" # react dev tools
      "cjpalhdlnbpafiamejdnhcphjbkeiagm" # ublock origin
      "gcbommkclmclpchllfjekcdonpmejbdp" # https everywhere
      "ldpochfccmkkmhdbclfhpagapcfdljkj" # decentraleyes: remember to search 'https everywhere decentraleyes'
      "jcgpghgjhhahcefnfpbncdmhhddedhnk" # click to remove element
      # "flnagcobkfofedknnnmofijmmkbgfamf" # url tracking and redirect skipper
      "kcpnkledgcbobhkgimpbmejgockkplob" # tracking token skipper
      "jaoafjdoijdconemdmodhbfpianehlon" # skip redirect
      "nomnklagbgmgghhjidfhnoelnjfndfpd" # canvas blocker
    ];
  };
  programs.firefox = {
    enable = true;
    # list here:
    # https://github.com/nix-community/nur-combined/blob/master/repos/rycee/pkgs/firefox-addons/addons.json
    extensions = with nur.repos.rycee.firefox-addons; [
      https-everywhere
      darkreader
      decentraleyes
      h264ify
      link-cleaner
      octotree
      old-reddit-redirect
      tree-style-tab
      tridactyl
      ublock-origin
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
        '';
        userContent = ''
          /* dark "unable to connect" tab */
          body.illustrated.connectionFailure.neterror {
            background-color: rgb(19,19,20);
            color: #a8a8a8;
          }
          body.illustrated.connectionFailure.neterror div.title h1 {
            color: #a8a8a8;
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

  home.keyboard.layout = "gb";
  # home.{language,currency,time,etc.}- see `man home-configuration.nix`

  # TODO: see `man home-configuration.nix`, `home.file.<name?>.onChange` for
  # xmonad reload? (Although, this seems like something that might be handled
  # by home-manager already?)
  xsession = {
    enable = true;
    windowManager.xmonad = {
      enable = true;
      enableContribAndExtras = true;
      config = ~/.dotfiles/xmonad.hs;
      # extraPackages = haskellPackages: [
      #   haskellPackages.xmonad-contrib
      #   haskellPackages.xmonad-extras
      # ];
      # haskellPackages = {};
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
    enable = true;
    profiles = {
      # TODO: read more here: https://github.com/rycee/home-manager/blob/master/modules/programs/autorandr.nix
      van = {
        fingerprint = {
          "eDP-1" = "00ffffffffffff004d108d1400000000051c0104a52213780ed920a95335bc250c5155000000010101010101010101010101010101014dd000a0f0703e803020350058c210000018000000000000000000000000000000000000000000fe00464e564452804c513135364431000000000002410328011200000b010a20200090";
          "DP-3" = "00ffffffffffff0010ace9a04c5937312a1d0103803d2378eeee95a3544c99260f5054a54b00714f8180a9c0a940d1c0e1000101010108e80030f2705a80b0588a00615d2100001a000000ff00464e38344b3941483137594c0a000000fc0044454c4c205532373138510a20000000fd0031560a893c000a202020202020016b02033ef15861605f5e5d10050402071601141f1213272021220306111523091f07830100006d030c001000307820006003020167d85dc40178c003e20f03565e00a0a0a0295030203500615d2100001a04740030f2705a80b0588a00615d2100001ebf1600a08038134030203a00615d2100001a00000000000000000000002e";
        };
        config = {
          "DP-3" = {
            enable = true;
            primary = true;
            position = "0x0";
            mode = "3840x2160";
            rate = "60.00";
            # dpi = 224;
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
            # dpi = 224;
            scale = {
              x = 1.3;
              y = 1.3;
            };
            # TODO: gamma? see `man home-configuration` and `xrandr --props`
            # TODO: put a scale-from command in the postswitch script?
          };
        };
      };
      docked = {
        fingerprint = {
          "DP-1-2" = "00ffffffffffff0005e369230d0200001a180104a5331d783ae595a656529d27105054bfef00d1c0b30095008180814081c001010101023a801871382d40582c4500fd1e1100001e000000fd00324c1e5311010a202020202020000000fc00323336394d0a20202020202020000000ff0041425045363941303030353235011702031ef14b901f051404130312021101230907078301000065030c0010008c0ad08a20e02d10103e9600fd1e11000018011d007251d01e206e285500fd1e1100001e8c0ad08a20e02d10103e9600fd1e110000188c0ad090204031200c405500fd1e1100001800000000000000000000000000000000000000000000000000fd";
          "DP-2-1" = "00ffffffffffff0005e36923b30200000c18010380331d782ae595a656529d27105054bfef00d1c0b30095008180814081c001010101023a801871382d40582c4500fd1e1100001e000000fd00324c1e5311000a202020202020000000fc00323336394d0a20202020202020000000ff004252534533394130303036393101a102031ef14b101f051404130312021101230907078301000065030c0020008c0ad08a20e02d10103e9600fd1e11000018011d007251d01e206e285500fd1e1100001e8c0ad08a20e02d10103e9600fd1e110000188c0ad090204031200c405500fd1e11000018000000000000000000000000000000000000000000000000006d";
          "eDP-1" = "00ffffffffffff004d108d1400000000051c0104a52213780ed920a95335bc250c5155000000010101010101010101010101010101014dd000a0f0703e803020350058c210000018000000000000000000000000000000000000000000fe00464e564452804c513135364431000000000002410328011200000b010a20200090";
        };
        config = {
          "DP-1-2" = {
            enable = true;
            primary = true;
            position = "0x0";
            mode = "1920x1080";
            rate = "60.00";
            # scale-from = "3840x2160";
            # dpi = 96;
            # scale = {
            #   x = 1.3;
            #   y = 1.3;
            # };
            # TODO: audio? see xrandr --props
          };
          "DP-2-1" = {
            enable = true;
            primary = false;
            position = "1920x0";
            mode = "1920x1080";
            rate = "60.00";
            # scale-from = "3840x2160";
            # dpi = 96;
            # scale = {
            #   x = 1.3;
            #   y = 1.3;
            # };
            # TODO: audio? see xrandr --props
          };
          # TODO: enable, but in 1920x1080? What happens if we enable in 3840x2160?
          "eDP-1".enable = false;
        };
      };
    };
    hooks = {
      postswitch = {
        "restart-xmonad" = "${config.xsession.windowManager.command} --restart"; # TODO: is this necessary? Try without..
        "change-dpi" = ''
          case "$AUTORANDR_CURRENT_PROFILE" in
            horizontal)
              DPI=96
              ;;
            # From https://topics-cdn.dell.com/pdf/xps-15-9570-laptop_specifications_en-us.pdf
            # Width:  344.21 mm (13.55 in)
            # Height: 193.62 mm (7.62 in)
            # Resolution: 3840x2160
            # DPI should be 283, but this is ugly
            undocked)
              DPI=224
              ;;
            docked)
              DPI=96
              ;;
            van)
              DPI=163
              ;;
            *)
              echo "Unknown profile: $AUTORANDR_CURRENT_PROFILE. Could not set DPI."
              exit 1
            esac

            echo "Xft.dpi: $DPI" | ${pkgs.xorg.xrdb}/bin/xrdb -merge
        '';
      };
    };
  };

  programs.broot = {
    enable = true;
    enableZshIntegration = true;
  };

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
      color.ui = "true";
    };
  };

  programs.zsh = {
    # TODO: migrating zshrc to here means it's possible to enforce dependencies. For example,
    # instead of aliasing 'kc' to 'kubectl', it's possible to alias 'kc' to
    # ${pkgs.kubectl}/bin/kubectl. However, this would mean reducing portability.
    enable = true;
    enableAutosuggestions = true;
    enableCompletion = true;
    # environment.pathsToLink = [ "/share/zsh" ];
    initExtra = builtins.readFile ~/.dotfiles/.zshrc;
    plugins = customZshPlugins;
    shellAliases = {
      b64 = "base64";
      b64d = "base64 --decode";
      chown = "chown -h";
      gacm = "git add -u; git commit -m";
      gau = "git add -u";
      gb = "git branch -lar";
      gcm = "git commit -m";
      gcob = "git checkout -b";
      gco = "git checkout";
      gcw = "git commit -m \"whatever\"";
      gdt = "git difftool";
      glns = "git log --name-status";
      gpu = "git pull";
      grohm = "git reset --hard origin/master";
      gst = "git status";
      gsti = "git status --ignored";
      hms = "home-manager switch";
      kcd = "kubectl delete";
      kcds = "kubectl describe";
      kce = "kubectl edit";
      kcgj = "kubectl get -o json";
      kcg = "kubectl get";
      kc = "kubectl";
      kclf = "kubectl logs -f";
      kcl = "kubectl logs";
      kclt = "kubectl logs -f --tail=0";
      kcpf = "kubectl port-forward";
      kcp = "kubectl patch";
      kcx = "kubectl exec";
      la = "ls -hAl";
      pg = "| grep";
      pkgsrch = "nix-env -f '<nixpkgs>' -qaP";
      scf = "systemctl --state=failed";
      sc = "systemctl";
      scur = "systemctl --user restart";
      scus = "systemctl --user status";
      scu = "systemctl --user";
      stripcolours="sed -r 's/\\x1B\\[([0-9]{1,2}(;[0-9]{1,2})?)?[mGK]//g'";
      vd = "nvim -d";
      v = "nvim";
      weather = "curl http://v2.wttr.in";
    };
  };

  programs.neovim = {
    enable = true;
    viAlias = true;
    vimAlias = true;
    # settings = ? # see programs.vim.settings and programs.vim.extraConfig
    configure = {
      # TODO: consider different colorschemes for different file types with autocommands?
      # TODO: move config out of .vim/after/plugins (or not? no harm in it being in different files
      # related to each plugin; and probably a little bit more portable outside of a nix or
      # nix-like system)
      customRC = (builtins.readFile ~/.dotfiles/init.vim) + "\n" + (filesIn ~/.dotfiles/.vim/after/plugin "vim");
      packages.myVimPackage = with pkgs.vimPlugins; {
        # list vim packages:
        # > nix-env -f '<nixpkgs>' -qaP -A vimPlugins
        start = with customVimPlugins; [
          auto-pairs
          awesome-vim-colorschemes
          dart-vim-plugin
          easymotion
          fugitive
          haskell-vim
          vim-indent-object
          LanguageClient-neovim
          markdown-preview
          ncm2
          ncm2-bufword
          ncm2-path
          ncm2-ultisnips
          # nvim-treesitter
          nvim-yarp # required for ncm2
          repeat
          rust-vim
          sensible
          sideways-vim
          solarized
          surround
          tcomment_vim
          # TODO: vim-textobj-comment # doesn't have 'vspec' file for modern vim plugins?
          typescript-vim
          ultisnips
          vim-airline
          vim-autoformat
          vim-flutter
          vim-go
          vim-gh-line
          vim-javascript
          vim-markdown
          vim-nix
          vim-toml
        ];
      };
    };
  };

  home.sessionVariables = {
    EDITOR = "vim";
    # TODO: this chromium instance has its data dir created at $BROWSER variable creation time, not
    # call time. Might need a wrapper script.
    # BROWSER = "chromium --incognito --user-data-dir=$(mktemp -d)";
    BROWSER = "firefox";
    TERMCMD = "alacritty";
    SSH_AUTH_SOCK="/run/user/$(id -u)/gnupg/S.gpg-agent.ssh";
  };

  # TODO: auto-restart services?
  # TODO: wait time for service shutdown (firefox isn't shutting down cleanly when the service is
  # stopped)
  systemd.user.services.firefox = basicService {
    desc = "Firefox";
    cmd = "${pkgs.firefox}/bin/firefox";
  };
  systemd.user.services.keybase-gui = basicService {
    desc = "Keybase GUI";
    cmd = "${pkgs.keybase-gui}/bin/keybase-gui";
    env = "NIX_SKIP_KEYBASE_CHECKS=1"; # TODO: Should probably investigate whether this is still necessary
  };
  systemd.user.services.xcompmgr = basicService { desc = "Xcompmgr"; cmd = "${pkgs.xorg.xcompmgr}/bin/xcompmgr -c"; };
  systemd.user.services.whatsapp = chromiumApp { name = "whatsapp"; desc = "WhatsApp Web"; url= "web.whatsapp.com"; };
  systemd.user.services.keep = chromiumApp { name = "keep"; desc = "Keep"; url = "keep.google.com"; };
  systemd.user.services.calendar = chromiumApp { name = "calendar"; desc = "Calendar"; url = "calendar.google.com"; };
  systemd.user.services.gmail = chromiumApp { name = "gmail"; desc = "Gmail"; url = "mail.google.com"; };
  systemd.user.services.hangouts = chromiumApp { name = "hangouts"; desc = "Hangouts"; url = "hangouts.google.com"; };
  systemd.user.services.fbmessenger = chromiumApp { name = "messenger"; desc = "Facebook Messenger"; url = "messenger.com"; };
  systemd.user.services.slack = constrainedService
    { desc = "Slack"; cmd = "${pkgs.slack-dark}/bin/slack"; env = "BROWSER=${pkgs.firefox}/bin/firefox"; };
  systemd.user.services.signal = constrainedService
    { desc = "Signal"; cmd = "${pkgs.signal-desktop}/bin/signal-desktop"; };
  systemd.user.services.spotify = constrainedService
    { desc = "Spotify"; cmd = "${pkgs.spotify}/bin/spotify"; };
  systemd.user.startServices = true;
  # From: https://nixos.wiki/wiki/Bluetooth#Using_Bluetooth_headset_buttons_to_control_media_player
  systemd.user.services.mpris-proxy = {
    Unit.Description = "Mpris proxy";
    Unit.After = [ "network.target" "sound.target" ];
    Service.ExecStart = "${pkgs.bluez}/bin/mpris-proxy";
    Install.WantedBy = [ "default.target" ];
  };

  home.packages = with pkgs; [
    ag
    alacritty # TODO: need to manage alacritty.yml with home manager
    arandr
    ascii
    # bingo
    # binutils-unwrapped
    bfg-repo-cleaner
    blueman
    cabal2nix
    calc
    cargo
    discord
    dmenu
    dnsutils
    docker-compose
    dos2unix
    exfat
    ffmpeg
    firefox
    fzy
    gcc
    ghostscript
    git
    gitAndTools.hub
    glxinfo
    gnumake
    gnumeric
    gnupg
    go
    # Check whether golang's official lang server implementation is available yet. Or perhaps use
    # this, per the advice on the gh page for the sourcegraph lang server implementation:
    # https://github.com/saibing/bingo. See the derivation earlier in this file for bingo.
    go-langserver
    godot
    graphviz
    jq
    keybase-gui
    kind
    # kubernetes-helm
    kubectl
    kustomize
    ldns # drill
    libreoffice
    libsecret
    lnav
    # lxrandr
    myNodePackages."newman-git://github.com/postmanlabs/newman#v4.5.7"
    mycli
    mysql
    mysql-workbench
    nmap
    myNode
    nodePackages.javascript-typescript-langserver
    nodePackages.node2nix
    openjdk
    openssh
    openssl
    openvpn
    pandoc
    pavucontrol
    pciutils
    plantuml
    postman
    python
    python3
    python37Packages.python-language-server
    python37Packages.sqlparse
    pwgen
    ripgrep
    rustc
    signal-desktop
    latestSkaffold
    shutter
    slack-dark
    socat
    spotify
    sqlite
    stack
    telnet
    texlive.combined.scheme-small # pdflatex for pandoc pdf output
    transmission # TODO: transmission service?
    tree
    vlc
    wireshark
    xclip
    xcompmgr
    xorg.xdpyinfo
    xsel
    yarn
    youtube-dl
    yq
    zip
    zoom-us
    # from here: https://github.com/yrashk/nix-home/commit/19bf8690b39e9d5747823dfbefee8d7e801205e1
    # and: https://github.com/NixOS/nixpkgs/issues/47608#issuecomment-443929385
    # TODO: the latter reports that using pkgs.unstable.zoom-us solves the problem. So worth
    # checking whether it's still a problem for newer versions.
    # (zoom-us.overrideAttrs (super: {
    #   postInstall = ''
    #     ${super.postInstall}
    #     wrapProgram $out/bin/zoom-us --set LIBGL_ALWAYS_SOFTWARE 1
    #   '';
    # }))

    dejavu_fonts
    inconsolata
    liberation_ttf
    powerline-fonts
    terminus_font
    ttf_bitstream_vera
    # vistafonts # marked as broken
  ];

  services.mpd.enable = true;
  services.unclutter.enable = true;
  services.keybase.enable = true;
  services.kbfs.enable = true;

  services.gpg-agent = {
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
    config = ~/.dotfiles/polybar/space.ini;
    script = "polybar top &";
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
  # https://github.com/b4b4r07/enhancd
  # Check config for other zsh modules
  # https://dougblack.io/words/zsh-vi-mode.html
  # https://terminalsare.sexy/
  # Check config for various vim plugins

  # TODO: convert vim config to lua
  # TODO: turn off zsh shared history
  # TODO: use systemd-networkd instead of networkmanager
  # TODO: handle URLs with a piece of software other than a browser. When a Github URL to source is
  #       detected open it in a terminal, in vim, instead of in the browser.
  # TODO: alias cp is annoying when it's expanded globally. Is it possible to use alias -m '^cp' to
  #       prevent that?
  # TODO: having the BROWSER variable create a new profile in /tmp every time the browser is
  #       started causes it to take a while to load and fill up the /tmp directory. Do something
  #       about this?
  # TODO: clear out /tmp periodically
  # TODO: vim jump-to-github from code line
  # TODO: configure BT earbuds to mute/unmute (in Pulseaudio?) when one of the headphone buttons is pressed
  # TODO: reminder utility like the one I wrote in the past
  # TODO: is there an alternative ls that shows git status in one of the columns?
  # TODO: get chromecast audio sink for pulseaudio
  # TODO: load dictionary into vim autocomplete when working on markdown files? (or would that just
  #       be annoying?)
  # TODO: Spotify control hotkeys
  # TODO: filter temporary directories out of enhancd `cd`?
  # TODO: check out `github` CLI
  # TODO: airplane mode? (rfkill??)
  # TODO: is it possible to stop Zoom from receiving window unfocus events? That way it might stay
  #       full-screened on whichever workspace it started on, like I'd prefer.
  # TODO: get that version of `man` with the examples
  # TODO: turn on vsync? should stop tearing when scrolling browser and watching videos
  # TODO: build a terminal in desktop flutter?
  # TODO: terminal + vim have switch between dark and light mode to make working in bright/dark
  #       environments easier.
  # TODO: I have a common pattern where I search for a kubernetes object then edit it. I should get
  #       a list of all deployed k8s objects (perhaps in a given namespace) and pass them to tv to
  #       select one to edit.
  # TODO: system-wide microphone amplitude cut-off (like Mumble and other talk apps).
  # TODO: hide firefox chrome? Or at least address bar. Maybe tabs (but then I'd have to keep my
  #       number of tabs under control).
  # TODO: resource-constrain Zoom
  # TODO: XMonad: "send this current window to the workspace occupied by that other window" via
  #       dmenu.
  # TODO: display current git branch (+status?) in terminal prompt
  # TODO: ephemeral terminal with `tv` options popped open so I can call `tv` directly from dmenu
  #       and have it disappear immediately after I close it.
  # TODO: disable mouse buttons other than left, right and middle/scroll.
  # TODO: clean up home directory. Stupid software that puts stuff there rather than ~/.config.
  #       Some of it might have env vars or options that prevent that.
  # TODO: add git information to prompt
  # TODO: is it possible to dark-mode all websites with some sort of color-palette transformer? In
  #       an addon so it's easy to toggle?
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
  # TODO: package kubefwd (should be pretty easy)
  # TODO: reminder framework that integrates with multiple devices and has cli. Perhaps a Keybase
  #       app?
  # TODO: if a sequence of subdirectories contains nothing, autocomplete to the depth of the first
  #       non-directory file, or fork in the tree.
  #       For example if the file ./some/directory/sequence/file.ext exists, but there are no files
  #       in parent directories, `cd` autocomplete should autocomplete the entire sequence.
  # TODO: put zoom, and meetings, on a specific workspace?
  # TODO: drop-down terminal? replace dmenu with this?
  # TODO: it's possible for the system to come out of hibernate and not be locked. This shouldn't
  #       be a problem, because the system state is saved to swap, which is encrypted and password
  #       protected. But it's worth thinking about whether this is a problem; what if I install
  #       this system to another machine without disk encryption?
  # TODO: some hotkey to go directly to enhancd instead of having to type 'cd -'
  # TODO: change all notes to markdown? Just set vim opts ft=md at the end?
  # TODO: can I blacklist domains in my browser so that I see links to them in black- indicating
  #       that they're terrible sites I never want to visit? I.e. yummly.
  # TODO: enhancd/fzy for opening recently opened files
  # TODO: some sort of text input in xmonad so I can type a search from anywhere and be taken to my
  #       browser, where that search/URL is executed
  # TODO: mic mute/unmute hotkey
  # TODO: wrap chromium with wrapProgram to enforce incognito, and set GDK_DPI_SCALE?
  #       | https://nixos.wiki/wiki/Nix_Cookbook#Wrapping_packages
  # TODO: auto-suspend at a certain battery level
  # TODO: figure out how to turn off all radios etc. for flight mode
  # TODO: Get FF addons in home-manager if possible
  #       | All Tabs Helper
  #       | CanvasBlocker
  #       | Decentraleyes
  #       | Hide Fixed Elements (or integrate this into tridactyl; probably possible with a script-inspect the Hide Fixed Elements code to see if it does anything other than add a css rule)
  #       | HTTPS Everywhere
  #       | Nuke Anything
  #       | React Developer Tools
  #       | Skip Redirect
  #       | Tracking Token Stripper
  #       | Tridactyl (see https://github.com/tridactyl/tridactyl#installing)
  #       | uBlock Origin
  #       | Wayback Machine
  # TODO: tridactyl
  #       | option to grayscale page when following hint. I.e. pressing f temporarily grays the page. This would mean using nicer colours for hints would be more feasible, as they wouldn't clash with pages.
  #       | multiple actions; i.e. 4j to move four "lines" down
  #       | hide fixed elements
  #       | enter/exit reader mode
  #       | tridactylrc
  #       | guiset/userChrome.css to control the chrome
  # TODO: make an easy key combo (comparable to <M-Return> for terminal) for opening a disposable chromium
  # TODO: tv
  #       | make a fancier `tv` to show a preview, if it exists?
  #       | allow deletion from the prompt?
  #       | provide option to cat the contents of a note to terminal instead of editing?
  #       | does not support path separator (forward-slash) in note names- replace automatically? error? support?
  #       | note content search?
  #       | namespacing? with directories? or is that better handled in the filename (with forward-slashes, even?)?
  #       | force creation of a new note: if I have a note called 'abc' and I want to create a note called 'ab' the current functionality does not allow this (try it)
  #       | allow command-line arguments as a seed to the search? advantage of this is that they'll go into shell command history
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
  # TODO: https://github.com/b4b4r07/enhancd#wrench-configurations
  #       increase the number of options shown for 'cd -'
  # TODO: update fetchFromGitHub package
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
  #       using paste mode might work?).
  # TODO: VIM: make a nicer keymapping for "next, previous difference" (currently `]c`, `[c`).
  #       Consider for example ;j and ;k (I don't use semicolon).
  # TODO: when the pointer comes out of hiding, try to make it extra visible. Perhaps do the mac
  #       thing where shaking the pointer makes it larger.
  # TODO: consider moving boot partition to USB drive
  # TODO: make sure the discrete graphics card is switched off
  # TODO: red shift toward the end of the day?
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
  # TODO: investigate how to shut firefox down cleanly as a service (probably just allow a certain
  #       amount of shutdown time in the service definition, and send a specific signal)
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
  # TODO: in status bar | indicator for internet connection status (TCP connection status?)
  #                     | DNS resolution status (i.e. can I resolve DNS right now?)
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
  #       pool. Then start all the google apps from the same chrome profile.
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
  # TODO: check whether programs.firefox.plugins exists yet
  # TODO: programs.fzf.enable = true; # probably not- have fzy now
  # TODO: programs.direnv.enable = true; # https://github.com/direnv/direnv/wiki/Nix
  # TODO: services.dunst.enable
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
  # as text?).
  # TODO: ligatures, especially for haskell
  #       https://github.com/tonsky/FiraCode
  #       https://www.google.com/search?q=vim%20haskell%20fira%20code
  #       https://www.hanselman.com/blog/MonospacedProgrammingFontsWithLigatures.aspx
  # TODO: if possible, change encryption to use: first) yubikey, second) otp, third) password?
  # https://www.google.com/search?q=luks%20multiple%20options%20for%20decryption
  # TODO: spotify cli with discovery features? Basically a recreation of the spotify ui in cli?
  # TODO: ag, sag/sack
  # TODO: pulseaudio-chromecast
  # TODO: put system config in version control
  # TODO: add some nix-specific instructions.. or a readme or something..
  # TODO: incorporate zshrc where appropriate
  # TODO: brightness control. xmonad? setxkbmap?
  # TODO: key binding to toggle touchpad, touchscreen on/off. Or just disable clicking with
  #       touchpad? Allow cursor movement? Is there any point (hur hur)?
  #       xinput disable $(xinput list | grep -i Touchpad | grep -o 'id=[0-9]\+' | grep -o '[0-9]\+')
  #       Is this necessary now that touchpad is disabled while typing?
  # TODO: get swipe on screen to scroll rather than select?
  # TODO: language server implementations: haskell-ide-engine, javascript, rust, cquery
  #       https://github.com/haskell/haskell-ide-engine#installation-with-nix
  #       https://langserver.org/
  #       https://nixos.wiki/wiki/Vim#Vim_as_a_Python_IDE
  # TODO: services.random-background.enable ?
  # TODO: services.redshift.enable ?
  # TODO: services.himawaripy.enable ? (might have to write this one..)
  # TODO: services.screen-locker.enable
  # TODO: automatically sleep/hibernate after some time (probably hibernate, for encryption/batt power)
  # TODO: low battery detection and notification
  # TODO: yi
  # TODO: map caps lock to escape?
  # TODO: put zsh history into sqlite db
  # TODO: change from oh-my-zsh to antigen. Or just nix-managed plugins?
  #       `man home-configuration.nix` has an example of this under programs.zsh.plugins
  # TODO: auto-dim screen, or apply power-saving methods automatically when external power is
  #       removed? And vice-versa?
  # TODO: add git config to cache private key password (or whatever it does) for a while, to avoid
  #       frequent re-entry
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
}

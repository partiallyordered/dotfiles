{ pkgs, lib, ... }:

let
  filesIn = with lib; with builtins; dir: suffix:
    foldl
      (a: b: a + "\n" + b)
      ""
      (attrValues
          (mapAttrs
            (name: _: readFile (dir + "/${name}"))
            (filterAttrs (name: type: hasSuffix ".${suffix}" name && type == "regular") (readDir dir))));

  basicService = desc: cmd:
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
        };
    };

  constrainedService = cpu: mem: desc: cmd:
    let s = basicService desc cmd;
    in
      s //
        {
          Service = s.Service // {
            CPUQuota = cpu;
            MemoryMax = mem;
          };
        };

  chromiumApp = name: desc: url:
    # TODO: is there a way to "deep-replace" in the output of
    # constrainedService instead of having the awkward .Service replacement
    # below?
    let s = constrainedService "150%" "2G" desc "${pkgs.chromium}/bin/chromium --app=https://${url} --class=${name} --user-data-dir=\$HOME/.config/chromium_${name}";
    in
      s //
        {
          Service = s.Service // {
            # need to escape nix and systemd unit file syntax
            ExecStartPre = "/run/current-system/sw/bin/mkdir -p \${HOME}/.config/chromium_${name}";
          };
        };

  customVimPlugins = {
    tcomment = pkgs.vimUtils.buildVimPlugin {
      name = "tcomment";
      src = pkgs.fetchFromGitHub {
        owner = "tomtom";
        repo = "tcomment_vim";
        rev = "fba729503bd0add6ccdea3d0a6f5ea8d0c942772";
        sha256 = "1sfkvs7921n6fck55clrh2g878cxr60l9ckgmxfznvwgy0wy25b2";
      };
    };
    sideways = pkgs.vimUtils.buildVimPlugin {
      name = "sideways";
      src = pkgs.fetchFromGitHub {
        owner = "AndrewRadev";
        repo = "sideways.vim";
        rev = "7c802da40d3a9b3d59c6e8141bf8d0ec737b1a74";
        sha256 = "1fxm8vhzdz3fzn1znka9c0gvz3yxcqqjjk4z0iy8gqy8v7qfpg3v";
      };
    };
    auto-pairs = pkgs.vimUtils.buildVimPlugin {
      name = "auto-pairs";
      src = pkgs.fetchFromGitHub {
        owner = "jiangmiao";
        repo = "auto-pairs";
        rev = "9086ce897a616d78baf69ddb07ad557c5ceb1d7c";
        sha256 = "02ds4i7aiq1a68qwz2gnmiigp25hi8qa9d4zcfazc3bgh855bx0l";
      };
    };
    indent-object = pkgs.vimUtils.buildVimPlugin {
      name = "indent-object";
      src = pkgs.fetchFromGitHub {
        owner = "michaeljsmith";
        repo = "vim-indent-object";
        rev = "5c5b24c959478929b54a9e831a8e2e651a465965";
        sha256 = "1kmwnz0jxjkvfzy06r7r73pcxfcyjp8p8m2d6qrhjfvzidgfhw19";
      };
    };
    # " TODO: this doesn't seem to recognise multiple single-line javascript comments (or perhaps
    # " single-line javascript comments at all). PR?
    # " OR: maybe it doesn't work with the 'ic' (i.e. 'in comment') object. Might just be best to get
    # " used to using 'ac' (i.e. 'around comment') object.
    # Plugin 'https://github.com/glts/vim-textobj-comment'
    # textobj-comment = pkgs.vimUtils.buildVimPlugin {
    #   name = "textobj-comment";
    #   src = pkgs.fetchFromGitHub {
    #     owner = "glts";
    #     repo = "vim-textobj-comment";
    #     rev = "58ae4571b76a5bf74850698f23d235eef991dd4b";
    #     sha256 = "00wc14chwjfx95gl3yzbxm1ajx88zpzqz0ckl7xvd7gvkrf0mx04";
    #   };
    # };
    # Plugin 'https://github.com/mxw/vim-jsx'
    # Plugin 'https://github.com/Raimondi/delimitMate/' # using auto-pairs now, is it better?
    # Plugin 'https://github.com/kana/vim-textobj-user'
  };

  customZshPlugins = [
    {
      # will source zsh-autosuggestions.plugin.zsh
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
  programs.home-manager.enable = true;
  programs.home-manager.path = https://github.com/rycee/home-manager/archive/master.tar.gz;
  programs.feh.enable = true;
  programs.htop.enable = true; # TODO: check out the config options you didn't know were there
  programs.chromium = {
    enable = true;
    extensions = [
      # TODO: privacy badger?
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

  home.keyboard.layout = "uk";
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
    };
    # TODO: but, but I just want to change the pointer size. Why do I have to
    # have this other stuff? Is there a default somewhere that I can override?
    pointerCursor.size = 64;
    pointerCursor.name = "Vanilla-DMZ";
    pointerCursor.package = pkgs.vanilla-dmz;
  };

  # TODO:
  # might need programs.autorandr.hooks.{..} (there are lots, see manual)
  programs.autorandr.enable = true;

  programs.git = {
    enable = true;
    userEmail = "mattkingston@gmail.com";
    userName = "msk-";
    extraConfig = {
      merge.tool = "vimdiff";
      mergetool.prompt = "true";
      # TODO: get rid of one of $LOCAL $REMOTE $MERGED? Don't really want three-way split. Can we
      # just use vimdiff2?
      "mergetool \"vimdiff\"".cmd = "nvim -d $LOCAL $REMOTE $MERGED -c '$wincmd w' -c 'wincmd J'";
      difftool.prompt = "false";
      diff.tool = "vimdiff2";
    };
  };

  programs.zsh = {
    enable = true;
    enableAutosuggestions = true;
    enableCompletion = true;
    # environment.pathsToLink = [ "/share/zsh" ];
    initExtra = builtins.readFile ~/.dotfiles/.zshrc;
    plugins = customZshPlugins;
    shellAliases = {
      b64 = "base64";
      vd = "nvim -d";
      la = "ls -hAl";
      sc = "systemctl";
      scu = "systemctl --user";
      scur = "systemctl --user restart";
      scus = "systemctl --user status";
      gau = "git add -u";
      gcm = "git commit -m";
      gcw = "git commit -m \"whatever\"";
      gdt = "git difftool";
      gst = "git status";
      kc = "kubectl";
      kce = "kubectl edit";
      kcg = "kubectl get";
      kcx = "kubectl exec";
      pg = "| grep";
      v = "vim";
      # tv = "vim $(/usr/bin/env ls ~/.dotfiles/notes/ | fzy)";
    };
  };

  programs.neovim = {
    enable = true;
    viAlias = true;
    vimAlias = true;
    # plugins = [ "ultisnips" "easymotion" "solarized" "languageClient-neovim" "youcompleteme" ];
    # settings = ? # see programs.vim.settings and programs.vim.extraConfig
    configure = {
      # TODO: consider different colorschemes for different file types with autocommands?
      customRC = (builtins.readFile ~/.dotfiles/init.vim) + "\n" + (filesIn ~/.dotfiles/.vim/after/plugin "vim");
      packages.myVimPackage = with pkgs.vimPlugins; {
        # list vim packages:
        # > nix-env -f '<nixpkgs>' -qaP -A vimPlugins
        start = with customVimPlugins; [
          auto-pairs
          awesome-vim-colorschemes
          easymotion
          haskell-vim
          indent-object
          "languageClient-neovim"
          repeat
          rust-vim
          sensible
          sideways
          solarized
          surround
          tcomment
          # TODO: textobj-comment # doesn't have 'vspec' file for modern vim plugins?
          ultisnips
          vim-go
          vim-javascript
          vim-nix
          vim-toml
        ];
      };
    };
  };

  home.sessionVariables = {
    WINIT_HIDPI_FACTOR = "1.7"; # scales alacritty
    EDITOR = "vim";
    # TODO: this chromium instance has its data dir created at $BROWSER variable creation time, not
    # call time. Might need a wrapper script.
    BROWSER = "chromium --incognito --user-data-dir=\\$(mktemp -d)";
    TERMCMD = "alacritty";
  };

  # TODO: auto-restart services?
  systemd.user.services.firefox = basicService "Firefox" "${pkgs.firefox}/bin/firefox";
  systemd.user.services.whatsapp = chromiumApp "whatsapp" "WhatsApp Web" "web.whatsapp.com";
  systemd.user.services.keep = chromiumApp "keep" "Keep" "keep.google.com";
  systemd.user.services.calendar = chromiumApp "calendar" "Calendar" "calendar.google.com";
  systemd.user.services.gmail = chromiumApp "gmail" "Gmail" "mail.google.com";
  systemd.user.services.hangouts = chromiumApp "hangouts" "Hangouts" "hangouts.google.com";
  systemd.user.services.signal = constrainedService "100%" "1G" "Signal" "${pkgs.signal-desktop}/bin/signal-desktop";
  systemd.user.services.spotify = constrainedService "100%" "1G" "Spotify" "${pkgs.spotify}/bin/spotify";
  systemd.user.startServices = true;

  home.packages = with pkgs; [
    ag
    alacritty # TODO: need to manage alacritty.yml with home manager
    blueman
    cargo
    dmenu
    firefox
    fzy
    gcc
    git
    glxinfo
    gnumake
    gnumeric
    helm
    jq
    kubectl
    libreoffice
    libsecret
    mysql
    mysql-workbench # for cli
    nmap
    nodejs
    openssh
    openvpn
    pavucontrol
    pciutils
    plantuml
    (polybar.override { pulseSupport = true; mpdSupport = true; githubSupport = true; })
    # pulseaudio-dlna
    python
    python3
    pwgen
    ripgrep
    rustc
    signal-desktop
    slack
    socat
    spotify
    stack
    tree
    vlc
    xorg.xbacklight
    xclip
    xsel
    zoom
    # TODO: yq, from here: https://github.com/mikefarah/yq
    # TODO: terminal_velocity. Find out what fzf, fzy or fasd does, as
    # `alias tv=fzf ~/.dotfiles/notes/`
    # could possibly replace terminal_velocity

    dejavu_fonts
    inconsolata
    liberation_ttf
    powerline-fonts
    terminus_font
    ttf_bitstream_vera
    vistafonts
  ];

  services.mpd.enable = true;
  services.unclutter.enable = true;

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

  # TODO: set up dnscrypt-proxy once it's upgraded past v2.
  #       https://developers.cloudflare.com/1.1.1.1/dns-over-https/cloudflared-proxy/
  #       - if possible use the ipv6 cloudflare resolvers; as the data from them may not be shared with APNIC
  #       https://nixos.org/nixos/manual/#sec-dnscrypt-proxy
  #       https://dnsleaktest.com/ (add result to status bar?)
  #       https://github.com/aarond10/https_dns_proxy
  #       https://github.com/NixOS/nixpkgs/issues/43298
  #       https://github.com/jedisct1/dnscrypt-proxy/wiki
  # TODO: set up wireguard interfaces to switch between vpn servers
  # TODO: run slock when the laptop closes, before suspend?
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
  # TODO: alacritty terminfo
  # TODO: is it worth network whitelisting certain processes?
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
  # TODO: get work calendar on personal calendar?
  # TODO: put firefox (work and personal) into systemd service?
  # TODO: in status bar | indicator for internet connection status (TCP connection status?)
  #                     | DNS resolution status (i.e. can I resolve DNS right now?)
  #                     | expected battery life, or usage rate?
  #                     | is the nvidia gpu on?
  #                     | screen brightness
  #                     | connected vpn name
  #                     | poll "Am I Mullvad"?
  #                     | whether the system is in a degraded state (systemctl status, systemctl --user status)
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
  # TODO: systemctl [--user] status; check the system isn't running degraded
  # TODO: put systemctl [--user] status in the status bar
  # TODO: https://nixos.org/nixos/manual/options.html#opt-services.logind.lidSwitchDocked
  # TODO: consider https://github.com/geommer/yabar
  # TODO: read nix pills https://nixos.org/nixos/nix-pills/
  # TODO: read manual: https://nixos.org/nix/manual/
  # TODO: check whether programs.firefox.plugins exists yet
  # TODO: programs.fzf.enable = true;
  # TODO: programs.direnv.enable = true;
  # TODO: services.dunst.enable
  # TODO: programs.noti.enable = true;
  # TODO: programs.taskwarrior.enable = true; # Or some equivalent
  # TODO: programs.rofi.enable; # consider, but it looks pretty heavy-weight..
  # TODO: systemd user service autorestart
  # TODO: use fzy for tab-autocompletion for zsh
  # TODO: tridactylrc
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
  # TODO: change from oh-my-zsh to antigen. Or just nix-managed plugins? `man
  # home-configuration.nix` has an example of this under programs.zsh.plugins
}

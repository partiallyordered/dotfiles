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
  constrainedService = cpu: mem: desc: cmd:
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
        CPUQuota = cpu;
        MemoryMax = mem;
        ExecStart = cmd;
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
in
{
  programs.home-manager.enable = true;
  programs.home-manager.path = https://github.com/rycee/home-manager/archive/master.tar.gz;
  programs.feh.enable = true;
  programs.htop.enable = true; # TODO: check out the config options you didn't know were there

  # TODO: check whether programs.firefox.plugins exists yet
  # TODO: programs.fzf.enable = true;
  # TODO: programs.direnv.enable = true;
  # TODO: programs.noti.enable = true;
  # TODO: programs.taskwarrior.enable = true; # Or some equivalent
  # TODO: programs.rofi.enable; # consider, but it looks pretty heavy-weight..

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
      config = /home/msk/.dotfiles/xmonad.hs;
    };
    # TODO: but, but I just want to change the pointer size. Why do I have to
    # have this other stuff? Is there a default somewhere that I can override?
    pointerCursor.size = 64;
    pointerCursor.name = "Vanilla-DMZ";
    pointerCursor.package = pkgs.vanilla-dmz;
  };

  # might need programs.autorandr.hooks.{..} (there are lots, see manual)
  programs.autorandr.enable = true;

  programs.git = {
    enable = true;
    userEmail = "mattkingston@gmail.com";
    userName = "msk-";
    extraConfig = {
      merge.tool = "vimdiff";
      mergetool.prompt = "true";
      # TODO: get rid of one of $LOCAL $REMOTE $MERGED? Don't really want three-way split.
      "mergetool \"vimdiff\"".cmd = "nvim -d $LOCAL $REMOTE $MERGED -c '$wincmd w' -c 'wincmd J'";
      difftool.prompt = "false";
      diff.tool = "vimdiff";
    };
  };

  programs.zsh = {
    enable = true;
    enableAutosuggestions = true;
    enableCompletion = true;
    # environment.pathsToLink = [ "/share/zsh" ];
    initExtra = ''
      '';
    oh-my-zsh = {
      enable = true;
      plugins = [ "git" "sudo" "cabal" "docker" "npm" "systemd" "vi-mode" ];
    };
    shellAliases = {
      vd = "nvim -d";
      la = "ls -hAl";
      sc = "systemctl";
      scu = "systemctl --user";
      gau = "git add -u";
      gcm = "git commit -m";
      gdt = "git difftool";
      gst = "git status";
      kc = "kubectl";
      kcg = "kubectl get";
      pg = "| grep";
      v = "vim";
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
      customRC = (builtins.readFile /home/msk/.dotfiles/init.vim) + "\n" + (filesIn /home/msk/.dotfiles/.vim/after/plugin "vim");
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
    BROWSER = "chromium --incognito --user-data-dir=$(mktemp -d)";
    TERMCMD = "alacritty";
  };

  # TODO: auto-restart services?
  systemd.user.services.whatsapp = chromiumApp "whatsapp" "WhatsApp Web" "web.whatsapp.com";
  systemd.user.services.keep = chromiumApp "keep" "Keep" "keep.google.com";
  systemd.user.services.calendar = chromiumApp "calendar" "Calendar" "calendar.google.com";
  systemd.user.services.gmail = chromiumApp "gmail" "Gmail" "mail.google.com";
  systemd.user.services.hangouts = chromiumApp "hangouts" "Hangouts" "hangouts.google.com";
  systemd.user.services.signal = constrainedService "100%" "1G" "Signal" "${pkgs.signal-desktop}/bin/signal-desktop";
  systemd.user.services.spotify = constrainedService "100%" "1G" "Spotify" "${pkgs.spotify}/bin/spotify";
  systemd.user.startServices = true;

  home.packages = with pkgs; [
    alacritty
    cargo
    chromium
    dmenu
    firefox
    git
    jq
    libreoffice
    nmap
    nodejs
    openssh
    pavucontrol
    polybar
    rustc
    signal-desktop
    slack
    socat
    spotify
    tree
    vlc
    xclip
    xsel
    # TODO: yq, from here: https://github.com/mikefarah/yq
    # TODO: terminal_velocity. Find out what fzf or fasd does, as `alias tv=fzf ~/.dotfiles/notes/`
    # could possibly replace terminal_velocity

    dejavu_fonts
    inconsolata
    liberation_ttf
    powerline-fonts
    terminus_font
    ttf_bitstream_vera
    vistafonts
  ];

  # services.compton = {
  #   enable = true;
  #   fade = true;
  # };

  services.unclutter.enable = true;

  # TODO: incorporate zshrc where appropriate
  # TODO: brightness control. xmonad? setxkbmap?
  # TODO: key binding to toggle touchpad on/off
  # TODO: language server implementations: haskell-ide-engine, javascript, rust
  #       https://github.com/haskell/haskell-ide-engine#installation-with-nix
  #       https://langserver.org/
  # TODO: services.dunst.enable
  # TODO: services.random-background.enable ?
  # TODO: services.redshift.enable ?
  # TODO: services.himawaripy.enable ? (might have to write this one..)
  # TODO: services.screen-locker.enable # get slock working first
  # TODO: automatically sleep/hibernate after some time (probably hibernate, for encryption/batt power)
  # TODO: low battery detection and notification
  # TODO: yi
  # TODO: map caps lock to escape?
  # TODO: put zsh history into sqlite db
  # TODO: change from oh-my-zsh to antigen. Or just nix-managed plugins? `man
  # home-configuration.nix` has an example of this under programs.zsh.plugins
}

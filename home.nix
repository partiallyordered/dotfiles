{ pkgs, ... }:

let
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
    # TODO: when we set --class on chromium, it works, but xmonad doesn't
    # recognise it; is this because chromium is setting the class after some
    # time, at which point xmonad has already assigned its workspace? While
    # working on this, it's worth noting that chromium windows opened with the
    # --app argument have WM_WINDOW_ROLE of 'pop-up'.
    # TODO: is there a way to "deep-replace" the output of constrainedService
    # instead of having the awkward .Service replacement below?
    let s = constrainedService "100%" "1G" desc "${pkgs.chromium}/bin/chromium --app=https://${url} --class=chromium-app --user-data-dir=\$HOME/.config/chromium_${name}";
    in
      s //
        {
          Service = s.Service // {
            # need to escape nix and systemd unit file syntax
            ExecStartPre = "/run/current-system/sw/bin/mkdir -p \${HOME}/.config/chromium_${name}";
          };
        };
in
{
  programs.home-manager.enable = true;
  programs.home-manager.path = https://github.com/rycee/home-manager/archive/master.tar.gz;

  home.keyboard.layout = "uk";

  xsession = {
    enable = true;
    windowManager.xmonad = {
      enable = true;
      enableContribAndExtras = true;
      # config = builtins.readFile "/home/msk/.dotfiles/xmonad.hs";
      # config = "/home/\$USER/.xmonad/xmonad.hs";
      config = "/home/msk/.dotfiles/xmonad.hs";
    };
  };

  programs.git = {
    enable = true;
    userEmail = "mattkingston@gmail.com";
    userName = "msk-";
  };

  programs.zsh = {
    enable = true;
    oh-my-zsh.enable = true;
  };

  programs.neovim = {
    enable = true;
    viAlias = true;
    vimAlias = true;
    configure = {
      customRC = ''
      '';
      packages.myVimPackage = with pkgs.vimPlugins; {
        start = [ youcompleteme easymotion ];
      };
    };
  };

  home.sessionVariables = {
    WINIT_HIDPI_FACTOR = "1.7"; # scales alacritty
    EDITOR = "vim";
    BROWSER = "chromium --incognito --user-data-dir=$(mktemp -d)";
  };

  systemd.user.services.whatsapp = chromiumApp "whatsapp" "WhatsApp Web" "web.whatsapp.com";
  systemd.user.services.gmail = chromiumApp "gmail" "Gmail" "mail.google.com";
  systemd.user.services.signal = constrainedService "100%" "1G" "Signal" "${pkgs.signal-desktop}/bin/signal-desktop";

  home.packages = with pkgs; [
    alacritty
    chromium
    dmenu
    firefox
    git
    htop
    jq
    libreoffice
    nmap
    nodejs
    pavucontrol
    signal-desktop
    slack
    socat
    spotify
    tree
    vlc
    xclip
    xsel

    dejavu_fonts
    inconsolata
    liberation_ttf
    powerline-fonts
    terminus_font
    ttf_bitstream_vera
    vistafonts
  ];
}

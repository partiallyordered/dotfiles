{ config, pkgs, lib, ... }:
# TODO: in status bar | indicator for internet connection status (TCP connection status? DNS,
#                     | result of "Am I Mullvad?"
#                     |   aggregate connectivity to various services; i.e. GH, messaging, email).
#                     |   systemd-networkd-wait-online.service might be useful here too
#                     |   see output of networkctl status; could use something from there
#                     | toggle for system notifications
#                     | git status
#                     | - password store
#                     | - dotfiles
#                     | - notes
#                     | clipboard control - GUI? clipmenu?
#                     | go to calendar workspace when clicking on date-time?
#                     | is there some way to characterise internet connectivity without abusing it?
#                     | BT audio connection - which headset is connected? - click/right-click to configure headset mode
#                     | click on wifi -> rofi menu -> select network
#                     | wifi network signal strength + speed (see `nmcli device wifi list`)
#                     | DNS resolution status (i.e. can I resolve DNS right now?)
#                     | used inodes above 70% (see the polybar filesystem module maybe??)
#                     | pueue status (pueue can push updates, and produce status as json)
#                     | expected battery life remaining, usage rate?
#                     | remaining battery life or time-until-charged
#                     | is the nvidia gpu on? # echo '\_SB.PCI0.PEG0.PEGP._OFF' > /proc/acpi/call
#                     | connected vpn name
#                     | whether the system is in a degraded state (systemctl status, systemctl --user status)
#                     | status of dotfile directory? status of working git repos? (did I forget to check something in?)
#                     | caps/num-lock?
#                     | touchpad on/off status/toggle?
#                     | touchscreen on/off status/toggle?
#                     | charging/discharging state
#                     | systemctl --user status xautolock AND hotkey/button to enable/disable xautolock
#                     | - perhaps show the text `LOCK` where no background indicates no problem,
#                     |   a click locks the screen (with loginctl, like in xmonad) and a red
#                     |   background indicates a problem- with a click perhaps displaying the
#                     |   problem in that case (or a middle click?). A right click could
#                     |   enable/disable the systemd services controlling locking.
#                     | the bar should automatically hide itself when something is full-screen,
#                     |   and automatically display itself when that thing goes away
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
#                     | Disk usage
#                     | Signal, Matrix, WhatsApp, Keybase, FB Messenger, Slack, gmail notifications?
#                     | Audio output being produced, audio input being received. I.e. a bar
#                     |   indicator showing the volume of audio being received at the mic and being
#                     |   produced at the speakers. Or something. The output volume might be obvious
#                     |   (should be able to hear it) and could be ignored, the input volume perhaps less so.
#                     | move GH notifications to notification manager
#                     | playing song
#                     | https://github.com/TiagoDanin/Awesome-Polybar
#                     | https://github.com/polybar/polybar-scripts
#                     | drop-down menu to restart services
#                     |   - in particular kanata, xmonad, because problems with these can hinder
#                     |     input/keyboard activity- so if the mouse is still working this can be
#                     |     an escape hatch
#                     |   - invoke sysz?
#                     | button to invoke earlyoom

{
  services.polybar = {
    enable = true;
    package = pkgs.polybar.override { pulseSupport = true; };
    # For some reason, these two script settings behave differently. I suppose the "implied"
    # polybar binary is one that's built with the options/modules defined here, instead of the
    # packaged one in pkgs.polybar.
    # script = "${pkgs.coreutils}/bin/nice -n15 ${pkgs.polybar}/bin/polybar top &";
    script = "${pkgs.coreutils}/bin/nice -n15 polybar top &";
    settings =
      let
        awk       = "${pkgs.gawk}/bin/awk";
        column    = "${pkgs.util-linux}/bin/column";
        terminal  = "${pkgs.alacritty}/bin/alacritty";
        mullvad   = "${pkgs.mullvad}/bin/mullvad";
        jq        = "${pkgs.jq}/bin/jq";
        dysk      = "${pkgs.dysk}/bin/dysk";
        sed       = "${pkgs.gnused}/bin/sed";
        grep      = "${pkgs.gnugrep}/bin/grep";
        rfkill    = "${pkgs.util-linux}/bin/rfkill";
        systemctl = "${pkgs.systemd}/bin/systemctl";
        shell     = "${pkgs.zsh}/bin/zsh";
        tr        = "${pkgs.coreutils-full}/bin/tr";
        watch     = "${pkgs.viddy}/bin/viddy -n 1";
      in {
        # Can probably use `let colors = { background = "#282A2E"; ...etc. }` and refer to colors
        # using nix syntax instead of polybar .ini syntax.
        "colors" = {
          background     = "#282A2E";
          background-alt = "#373B41";
          foreground     = "#C5C8C6";
          primary        = "#F0C674";
          secondary      = "#8ABEB7";
          alert          = "#A54242";
          disabled       = "#707880";
        };

        "bar/top" = {
          width                = "100%";
          height               = "2%";
          radius               = "6";

          background           = "\${colors.background}";
          foreground           = "\${colors.foreground}";

          line-size            = "3pt";

          border-size          = "4pt";
          border-color         = "#00000000";

          padding-left         = "0";
          padding-right        = "1";

          module-margin        = "1";

          separator            = "|";
          separator-foreground = "\${colors.disabled}";

          # Polybar font documentation: https://github.com/polybar/polybar/wiki/Fonts
          # Find material design icons and codepoints (perhaps update the version to match https://www.npmjs.com/package/@mdi/font):
          #   https://pictogrammers.github.io/@mdi/font/6.6.96/
          # There are also:
          # - gucharmap - https://github.com/polybar/polybar/wiki/Fonts#gnome-character-map
          # - nix-shell -p fontforge-gtk --command "fontforge $(fc-list | sk | cut -d: -f1)"
          # - https://www.nerdfonts.com/cheat-sheet
          # - https://fontdrop.info/#/?darkmode=true (weak search functionality, find a font to load with fc-list | grep -i <font-name>)
          # - https://beautifulwebtype.com/fira-code/glyphs/?i=5
          # - https://mathew-kurian.github.io/CharacterMap/
          # - https://fontawesome.com/v5/cheatsheet
          # - https://github.com/Keyamoon/IcoMoon-Free
          # - https://feathericons.com/ | https://github.com/AT-UI/feather-font
          # - https://github.com/lukas-w/font-logos
          # Insert unicode codepoints in vim in insert mode using C-V then
          # - for a codepoint smaller than u00001 press u (lower case u) then enter the codepoint padded with leading zeroes to 4 chars in length
          # - for a codepoint greater than uffff press U (upper case u) then enter the codepoint padded with leading zeroes to 8 chars in length
          # Fonts are defined using <font-name>;<vertical-offset>
          # Font names are specified using a fontconfig pattern.
          #   font-0 = NotoSans-Regular:size=8;2
          #   font-1 = MaterialIcons:size=10
          #   font-2 = Termsynu:size=8;-1
          #   font-3 = FontAwesome:size=10
          # Disable antialiasing:
          #   font-0 = NotoSans-Regular:size=8:antialias=false;2
          font = [
            "FiraCode Nerd Font:size=11;2"
            "Material Design Icons:size=11;2"
            "DejaVu Sans:size=11;2"
            "DejaVu Sans Mono wifi ramp:size=11;2"
            "igrowl\\\\-steadysets:size=11;2"
          ];

          # TODO: move workspaces, window to a different bar? Decide after the tidy-up gets applied.
          # TODO: replace long workspace names with icons
          # TODO: replace application-specific workspaces with "email" "IM" etc.
          modules-left         = "xworkspaces";
          # TODO: use modules center for window name (or remove window name altogether). Put notifications in
          #       a notification handler.
          modules-center       = "";
          modules-right        = "screen-lock systemd-user systemd-system filesystem inode-usage mullvad-dns pulseaudio memory swap temp cpu mullvad bluetooth wlan-rfkill wlan date backlight battery";

          cursor-click         = "pointer";
          cursor-scroll        = "ns-resize";

          enable-ipc           = true;
        };

        "module/temp" = {
          type = "internal/temperature";
          hwmon-path = "/sys/devices/platform/coretemp.0/hwmon/hwmon6/temp1_input";
        };

        "module/xworkspaces" = {
          type                    = "internal/xworkspaces";

          reverse-scroll          = true;

          # Unfortunately, we need to manually keep this up to date with the XMonad workspaces config
          icon = [
            "firefox;󰈹"
            "1;1"
            "2;2"
            "3;3"
            "4;4"
            "5;5"
            "6;6"
            "7;7"
            "8;8"
            "9;9"
            "0;0"
            "-;-"
            "=;="
            "BS;BS"
            "INS;INS"
            "HOME;HOME"
            "PGUP;PGUP"
            "zoom;󰍫"
            "whatsapp;󰖣"
            "gmail;󰊫"
            "protonmail;󰇱"
            "calendar;󰸘"
            "contacts;󰛋"
            "signal;󰍡"
            "spotify;󰓇"
            "zeal;Z"
            "chromium;󰊯"
            "slack;󰒱"
            "thunderbird;󰇮"
            "freetube;󰗃"
          ];

          format                  = "<label-state>";
          format-font             = "2";
          label-active            = "%icon%";
          label-active-background = "\${colors.background-alt}";
          label-active-underline  = "\${colors.primary}";
          label-active-padding    = "1";

          label-occupied          = "%icon%";
          label-occupied-padding  = "1";

          label-urgent            = "%icon%";
          label-urgent-background = "\${colors.alert}";
          label-urgent-padding    = "1";

          label-empty             = "";
        };

        "module/filesystem" = {
          # We want to display a warning when the mount is highly used, but not display anything otherwise
          type                       = "internal/fs";
          interval                   = "25";

          mount-0                    = "/";
          mount-1                    = "/boot";

          label-mounted              = "";

          format-warn                = "<label-warn>";
          warn-percentage            = "85";
          label-warn                 = "%mountpoint%: WARNING %percentage_used%% used";

          label-unmounted            = "";
          label-unmounted-foreground = "";
        };

        "module/pulseaudio" = {
          type                            = "internal/pulseaudio";

          format-volume-prefix-foreground = "\${colors.primary}";
          format-volume                   = "<ramp-volume> <label-volume>";
          ramp-volume                     = ["󰕿" "󰖀" "󰕾"];
          label-volume                    = "%percentage:3%%";

          format-muted-foreground         = "\${colors.disabled}";
          format-muted                    = "󰖁 <label-volume>";

          # TODO: start this window floating- might require
          # - specifying alacritty --class
          # - configuring xmonad to float the window (or finding out what xmonad uses to determine whether to float the window)
          # - specifying the window position, width, height
          #   - this could be with --embed, or with --config-file
          #     - https://www.google.com/search?hl=en&q=simple%20x11%20embed%20wrapper%20cli
          #   - https://github.com/alacritty/alacritty#configuration
          # - forcing focus on this window?
          # - https://github.com/noctuid/tdrop
          # - https://github.com/rust-x-bindings/rust-xcb
          #   - https://github.com/rust-x-bindings/toy_xcb
          # - https://wiki.tcl-lang.org/page/How+to+embed+a+non-Tk+GUI+into+a+Tk+frame
          # - https://rosettacode.org/wiki/Window_creation/X11
          # - https://docs.rs/tao/latest/tao/
          click-right                     = "${pkgs.alacritty}/bin/alacritty -e ${pkgs.ncpamixer}/bin/ncpamixer";
        };

        "module/memory" = {
          type                     = "internal/memory";
          interval                 = "2";
          format-prefix            = "RAM ";
          format-prefix-foreground = "\${colors.primary}";
          label                    = "%percentage_used:2%%";
        };

        "module/swap" = {
          type                     = "internal/memory";
          interval                 = "2";
          format-prefix            = "SWAP ";
          format-prefix-foreground = "\${colors.primary}";
          label                    = "%percentage_swap_used:2%%";
        };

        "module/cpu" = {
          type                     = "internal/cpu";
          interval                 = "2";
          format-prefix            = "CPU ";
          format-prefix-foreground = "\${colors.primary}";
          label                    = "%percentage-sum:3%%";
        };

        "module/backlight" = {
          type                  = "internal/backlight";

          # Use the following command to list available cards:
          # $ ls -1 /sys/class/backlight/
          card                  = "intel_backlight";

          # Use the `/sys/class/backlight/.../actual-brightness` file
          # rather than the regular `brightness` file.
          # Defaults to true unless the specified card is an amdgpu backlight.
          # New in version 3.6.0
          use-actual-brightness = "true";

          # Enable changing the backlight with the scroll wheel
          # NOTE: This may require additional configuration on some systems. Polybar will
          # write to `/sys/class/backlight/${self.card}/brightness` which requires polybar
          # to have write access to that file.
          # DO NOT RUN POLYBAR AS ROOT. 
          # The recommended way is to add the user to the
          # `video` group and give that group write-privileges for the `brightness` file.
          # See the ArchWiki for more information:
          # https://wiki.archlinux.org/index.php/Backlight#ACPI
          # Default: false
          enable-scroll         = "true";

          format                = "<ramp> <label>";

          # Only applies if <ramp> is used
          ramp = [ "🌕" "🌔" "🌓" "🌒" "🌑" ];
        };

        "network-base" = {
          type                = "internal/network";
          interval            = "1";
          format-connected    = "<label-connected>";
          format-disconnected = "<label-disconnected>";
          label-disconnected  = "%{F#F0C674}%ifname%%{F#707880} disconnected";
        };

        "module/wlan" = {
          "inherit"             = "network-base";
          interface-type        = "wireless";
          format-connected      = "<ramp-signal> <label-connected>";
          # Override the default- it seems that once a single packet has been lost the
          # format-packetloss string will be displayed forever more. This might be okay-ish for a
          # wired connection, but is a bit annoying for a wifi connection. (And I might in fact be
          # wrong about this..).
          format-packetloss     = "<ramp-signal> <label-connected>";
          # May need to change this when either updating fonts or when changing ramp icons
          format-connected-font = 2;
          label-connected       = "%{F#F0C674}%ifname%%{F-} %essid% 󰕒 %upspeed:5% 󰇚 %downspeed:5%";

          ping-interval         = 3;
          speed-unit            = "";

          # Should work with pretty much any font:
          # Can be useful to append these to the others to see what's being displayed
          # ramp-signal = [ "------" "+-----" "++----" "+++---" "+++++-" "++++++" ];

          # Material design icons:
          ramp-signal = [ "󰤯" "󰤟" "󰤢" "󰤥" "󰤨" ];

          # DejaVu Sans Mono wifi ramp:
          # From the example, ramp-signal-0 here is the "disconnected" symbol; is that correct? Is it
          # misleading? In fact, the ramp-signal lists don't need to be six items long, they can be
          # any length.
          # ramp-signal =  [ "" "" "" "" "" "" ];

          # igrowl-steadysets:
          # ramp-signal = [ "" "" "" "" "" ];
        };

        "module/date" = {
          type             = "internal/date";
          interval         = "1";

          date             = "%Y-%m-%d %H:%M";

          label            = "%date%";
          label-foreground = "\${colors.primary}";
        };

        # TODO: we should highlight this when it's not connected
        "module/mullvad" = {
          type         = "custom/script";
          exec         = "echo $(${mullvad} status | ${awk} '{print $1}') $(${mullvad} relay get | ${sed} 's/^.*in country \\\\([^ ]*\\\\) .*$/\\\\1/')";

          click-left   = "${mullvad} connect";
          click-right  = "${config.home.homeDirectory}/${config.home.file.select-mullvad-country.target} && ${mullvad} connect";
          click-middle = "${mullvad} disconnect";

          interval     = "2";
          label        = "%output%";
          format       = "󰖂 <label>";
        };

        "alert" = {
          type                   = "custom/script";

          format                 = "ALERT: <label>";
          format-prefix          = " ";
          format-suffix          = " ";
          format-background      = "\${colors.alert}";

          format-fail            = "ALERT: <label>";
          format-fail-prefix     = " ";
          format-fail-suffix     = " ";
          format-fail-background = "\${colors.alert}";
        };

        "module/mullvad-dns" = {
          "inherit"   = "alert";
          interval    = "5";
          exec        = "if [[ $(${mullvad} dns get | ${grep} -c '\\\\(ads\\\\|trackers\\\\|malware\\\\): true') -ne 3 ]]; then echo uh oh; else echo -e '\\\\n'; fi";
          label       = "VPN DNS misconfigured";
          click-left  = "${terminal} -e ${shell} -ic \"${watch} -- ${mullvad} dns get\"";
        };

        "module/inode-usage" =
          let
            alert_percentage = "85";
          in {
            "inherit"   = "alert";
            interval    = "60";
            exec        = "VAL=\"$(${dysk} -j | ${jq} '.[] | select(.\"mount-point\" == \"/\") | .stats.inodes.\"used-percent\"' | ${tr} -d '%\"')\"; if [[ $VAL -gt ${alert_percentage} ]]; then echo $VAL; else echo -e '\\\\n'; fi";
            click-left  = "${terminal} -e ${shell} -ic \"${watch} -- ${dysk} -c +inodes_use_percent\"";
            label       = "inode usage: %output%%";
          };

        # TODO: handle
        # - timer units that are failing
        # - units that should be running but have been stopped
        # - units that should be running but haven't been loaded
        #   specifically, there's a difference between the output of systemctl list-units and
        #   systemctl list-unit-files. One lists services that have been loaded into memory, the
        #   other, unit files.
        "module/systemd-system" = {
          "inherit"  = "alert";
          interval   = "10";
          exec       = "if ${systemctl} --output=json --failed | ${jq} -e 'length == 0' > /dev/null; then echo -e '\\\\n'; else echo uh oh; fi";
          click-left = "${terminal} -e ${shell} -ic \"${watch} -- ${systemctl} --failed\"";
          label      = "systemd degraded";
        };

        "module/systemd-user" = {
          "inherit"  = "alert";
          interval   = "10";
          exec       = "if ${systemctl} --user --output=json --failed | ${jq} -e 'length == 0' > /dev/null; then echo -e '\\\\n'; else echo uh oh; fi";
          click-left = "${terminal} -e ${shell} -ic \"${watch} -- ${systemctl} --user --failed\"";
          label      = "systemd user degraded";
        };

        "module/screen-lock" = {
          "inherit"  = "alert";
          interval   = "1";
          exec       = "if ! ${systemctl} --output=json --user list-units *.service | ${jq} -e '[.[] | select(.unit == \"xss-lock.service\" or .unit == \"xautolock-session.service\")] | length | . == 2' > /dev/null; then echo 'uh oh'; else echo -e '\\\\n'; fi";
          click-left =
            let services = "xautolock-session.service xss-lock.service";
            in "${terminal} -e ${shell} -ic \"echo Attempting start; ${systemctl} --user start ${services}; ${systemctl} --user status ${services}\"";
          label      = "screen lock not functioning";
        };

        "module/wlan-rfkill" = {
          type        = "custom/script";
          interval    = "1";
          label       = "%output:1%";
          exec        = "if ${rfkill} -J | ${jq} -e '.rfkilldevices[] | select(.type == \"wlan\") | .soft == \"unblocked\"' > /dev/null; then echo '󰖩'; else echo '󰖪'; fi";
          click-right = "${terminal} -e ${shell} -ic \"${watch} -- ${rfkill}\"";
          click-left  = "${rfkill} toggle wlan";
        };

        "module/bluetooth" = {
          type         = "custom/script";
          interval     = "1";
          label        = "%output:1%";
          click-left   = "${shell} -c \"${rfkill} unblock bluetooth; ${config.home.homeDirectory}/${config.home.file.bt-conn.target}\"";
          click-middle = "${terminal} -e ${shell} -ic \"${systemctl} list-units bluetooth; ${rfkill}; read\"";
          click-right  = "${rfkill} toggle bluetooth";
          # Why print the symbol we want, instead of using label-fail, format-fail, etc.? Well, at
          # the time of writing, there was a pause in the output when running `systemctl stop
          # bluetooth`, meaning the bluetooth icon blinked out of existence for a moment. I didn't
          # determine the cause of the icon disappearance, instead I developed this hack, which
          # works reliably.
          exec = lib.strings.concatStringsSep " " [
            "if"
              "${rfkill} -J | ${jq} -e '.rfkilldevices[] | select(.type == \"bluetooth\") | .soft == \"unblocked\"' > /dev/null &&"
              "${systemctl} is-active bluetooth > /dev/null;"
            "then"
              "if [ $(echo info | ${pkgs.bluez}/bin/bluetoothctl | ${grep} -c 'Device') -eq 0 ]; then"
                "echo '󰂯';"
              "else"
                "echo '%{F#2193ff}󰂯';"
              "fi"
            "else"
              "echo '%{F#555}󰂲';"
            "fi"
          ];
        };

        "module/battery" = {
          type                  = "internal/battery";
          battery               = "BAT0";
          adapter               = "AC";

          format-discharging    = "<ramp-capacity> <label-discharging>";
          format-charging       = "<ramp-capacity> <label-charging>";
          format-full           = "<ramp-capacity> <label-full>";
          label-discharging     = "%percentage%% -%consumption:2:2%W %time%";
          label-charging        = "%percentage%% +%consumption:2:2%W %time%";
          label-full            = "%percentage%%";

          time-format           = "%H:%M";
          ramp-capacity         = [ "󰁺" "󰁻" "󰁼" "󰁽" "󰁾" "󰁿" "󰂀" "󰂁" "󰂂" "󰁹" ];

          poll-interval         = 1;

          # At the time of writing, the battery needs replacing and the laptop power usage needs
          # addressing.
          low-at                = "15";
          # format-low is not displayed when the power is plugged in, so we can use
          # label-discharging here
          format-low            = "<ramp-capacity> <label-discharging>";
          format-low-background = "\${colors.alert}";
        };

        "settings" = {
          screenchange-reload = true;
          pseudo-transparency = true;
        };
      };
  };
}

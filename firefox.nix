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
    # TODO: counter to the following two TODOs: remove as many addons as possible to maximise
    # security (though do consider that most actual browsing doesn't have any logged-in sessions so
    # this can probably leak information- but preferably wouldn't anyway...)
    # TODO: https://addons.mozilla.org/en-US/firefox/addon/har-json-viewer/
    # TODO: https://addons.mozilla.org/en-US/firefox/addon/tabcenter-reborn/
    # TODO: tree-style tabs; or sidebery (the new version..?)
    loadtabonselect3 = buildFirefoxXpiAddon rec {
      pname = "loadtabonselect3";
      version = "1.2022.504.1";
      addonId = "{85d2532b-a793-4048-8ef4-713af1ff320d}";
      url = "https://addons.mozilla.org/firefox/downloads/file/3944644/loadtabonselect3-${version}.xpi";
      sha256 = "0ja94wasr1s6gwxydyjk7ivqg4ns1a85hwljpbp3897kpy6nx5fq";
    };
    hide-fixed-elements = buildFirefoxXpiAddon {
      pname = "hide-fixed-elements";
      version = "1.2";
      addonId = "hidefixedelements@commonground.systems";
      url = "https://addons.mozilla.org/firefox/downloads/file/832286/hide_fixed_elements-1.2-an+fx.xpi";
      sha256 = "05mzmddd9kql64kn29vp8f2kznfzc4fjp4qz7m88syi9snxkkdjg";
    };
  };

  extensions = with pkgs.nur.repos.rycee.firefox-addons; with myFirefoxAddons; [
    # TODO: noscript
    bitwarden
    browserpass
    darkreader
    decentraleyes
    firefox-translations
    link-cleaner
    old-reddit-redirect
    private-relay
    react-devtools
    redirector
    search-by-image
    skip-redirect
    tridactyl
    ublock-origin
    wayback-machine
    sidebery

    loadtabonselect3
    hide-fixed-elements
  ];
in
  {
    # TODO: this doesn't seem to be loading tridactylrc
    package = pkgs.firefox.override {
      cfg = {
        # TODO: enableBrowserpass?
      };
      # TODO: https://hensm.github.io/fx_cast/
      nativeMessagingHosts = [ pkgs.tridactyl-native ];
    };
    enable = true;
    # list here:
    # https://github.com/nix-community/nur-combined/blob/master/repos/rycee/pkgs/firefox-addons/addons.json
    # Useful: https://ffprofile.com/
    profiles =
      let
        settingsShared = {
          # TODO: would be good if possible to get the "firefoxService" services to open all links
          #       in select-browser. So that if I click a link in say, WhatsApp, it's opened in
          #       select-browser. Perhaps though, it's better to use the Matrix-Element bridge and
          #       ditch the Whatsapp browser app? Consider paying for Element One?
          #       - https://github.com/mautrix/whatsapp
          #       - https://matrix.org/docs/guides/whatsapp-bridging-mautrix-whatsapp
          #       - can we use some sort of link rewriting (perhaps one that I create myself) to
          #         rewrite all links to have a different scheme than http or https? So for
          #         example, https://google.com would be rewritten to external:https://google.com,
          #         then it would be possible to configure FF to open `external:` links with
          #         xdg-open (or similar).
          # TODO: can/should we configure some search engines here?
          #         see: https://wiki.archlinux.org/title/Firefox#Adding_search_engines
          #       - e.g. the "http" search engine for http response codes?
          # TODO: https://wiki.archlinux.org/title/Firefox/Tweaks
          # TODO: set default search engine to not-Google
          # TODO: disable all telemetry
          # TODO: turn on https only mode for all kiosks
          # TODO: remove history for kiosks
          # TODO: disable autofill credit cards
          # TODO: https://amiunique.org/fp
          #       - hide browser version
          #       - hide user agent?
          #       - hide font list from server?
          #       - disable WebGL by default?

          ###
          # https://wiki.archlinux.org/title/Firefox/Privacy#Change_user_agent_and_platform
          ###
          # > [The following value] is used as the user agent for the Tor browser, thus being very
          # > common.
          "general.useragent.override" = "Mozilla/5.0 (Windows NT 10.0; rv:102.0) Gecko/20100101 Firefox/102.0";
          # > Tip: The value Win32 is used as the platform for the Tor browser, corresponding with
          # > the user agent provided above.
          "general.platform.override" = "Win32";
          # > WebGL is a potential security risk.[1] Set webgl.disabled to true in about:config if
          # > you want to disable it. (It's also used for fingerprinting).
          "webgl.disabled" = true;
          ###

          # Set to never try to store any credentials
          # TODO: go through the existing creds in the default profile and move them to pass where
          #       appropriate
          "signon.rememberSignons" = false;

          # TODO: not yet working:
          # https://wiki.archlinux.org/title/Firefox#Dark_themes
          "ui.systemUsesDarkTheme" = 1;
          # TODO: set in Firefox 100
          "layout.css.prefers-color-scheme.content-override" = 0;

          # Disable disk cache, enable memory cache. This is because the FF disk cache uses a lot
          # of inodes, which is a problem on my f2fs machine.
          "browser.cache.disk.enable" = false;
          "browser.cache.memory.enable" = true;

          # TODO: find setting to stop firefox hassling me about default browser
          "browser.shell.checkDefaultBrowser" = false;
          # Do not confirm window close before quitting
          "browser.warnOnQuit" = false;
          "browser.menu.showViewImageInfo" = true;
          "browser.search.region" = "GB";
          "browser.search.isUS" = false;
          "distribution.searchplugins.defaultLocale" = "en-GB";
          "general.useragent.locale" = "en-GB";
          "browser.bookmarks.showMobileBookmarks" = true;
          # https://old.reddit.com/r/firefox/comments/fyqrd7/new_tab_in_dark_mode/fn1mt4f/
          # https://gist.github.com/gmolveau/a802ded1320a7591a289fb7abd0d6c45
          "toolkit.legacyUserProfileCustomizations.stylesheets" = true;
          # https://wiki.mozilla.org/Privacy/Privacy_Task_Force/firefox_about_config_privacy_tweeks
          # https://news.ycombinator.com/item?id=31480950
          "privacy.firstparty.isolate" = true;
          # TODO: about privacy.resistFingerprinting:
          #         Mozilla does not recommend users enable it, since it will break a few websites
          #         (e.g. favicons may not load, pages will feel sluggish).
          #       Can set a bunch of stuff that it comprises of manually though. See here:
          #       https://wiki.archlinux.org/title/Firefox/Privacy#Anti-fingerprinting
          #
          #       https://wiki.mozilla.org/Security/Fingerprinting
          # privacy.resistFingerprinting is disabled because:
          # - it means websites will prefer light theme
          # - seems to complicate clipboard usage
          "privacy.resistFingerprinting" = false;
          "dom.battery.enabled" = false;
          "dom.event.clipboardevents.enabled" = false;
          "geo.enabled" = false;
          "media.navigator.enabled" = false;
          "network.http.referer.trimmingPolicy" = 2;
          "extensions.activeThemeID" = "firefox-compact-dark@mozilla.org";
          "browser.aboutConfig.showWarning" = false;
          "extensions.pocket.enabled" = false;
        };
        settingsKiosk = settingsShared // {
          "identity.fxaccounts.enabled" = false;
          "privacy.clearOnShutdown.cache" = true;
          "privacy.clearOnShutdown.cookies" = true;
          "privacy.clearOnShutdown.downloads" = true;
          "privacy.clearOnShutdown.formdata" = true;
          "privacy.clearOnShutdown.history" = true; # TODO: does not seem to be working
          "privacy.clearOnShutdown.openWindows" = true;
          "privacy.clearOnShutdown.offlineApps" = true;
          # generally, I trust things I run in kiosk mode more than I trust general browsing, and
          # functional clipboard usage is more important for these applications
          "dom.event.clipboardevents.enabled" = true;
          # don't want to have to log in to this stuff every time
          # TODO: can I whitelist site settings, cookies, etc. per-profile? I.e. retain only Slack
          # settings, cookies etc. for the Slack profile?
          "privacy.clearOnShutdown.siteSettings" = false;
          "privacy.sanitize.sanitizeOnShutdown" = false;
          "privacy.clearOnShutdown.sessions" = false;
        };
        settingsMain = settingsShared // {
          "privacy.resistFingerprinting" = true;
        };
        # https://www.userchrome.org/
        # https://www.userchrome.org/find-user-style-recipes.html
        # https://firefox-source-docs.mozilla.org/devtools-user/browser_toolbox/index.html
        # https://old.reddit.com/r/firefox/comments/fyqrd7/new_tab_in_dark_mode/fn1mt4f/
        # https://gist.github.com/gmolveau/a802ded1320a7591a289fb7abd0d6c45
        # https://github.com/mbnuqw/sidebery/wiki/Firefox-Styles-Snippets-(via-userChrome.css)
        userChrome = ''
          tabbrowser tabpanels { background-color: rgb(19,19,20) !important; }
          browser { background-color: rgb(19,19,20) !important; }
          /* Replace the white flash before a page loads */
          :root {
            --in-content-page-background: rgb(19,19,20) /*flash on new tab*/
          }
          #TabsToolbar
          {
              visibility: collapse;
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
        userChromeKiosk = ''
          ${userChrome}
          #navigator-toolbox { visibility: collapse; }
        '';
        # http://kb.mozillazine.org/index.php?title=UserContent.css
        # https://davidwalsh.name/firefox-user-stylesheet
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
        configKiosk = {
          inherit userContent;
          userChrome = userChromeKiosk;
          settings = settingsKiosk;
        };
      in {
        # Take note, the .id property needs to be sequential
        default = {
          inherit userChrome userContent extensions;
          settings = settingsMain;
          id       = 0;
        };
        app        = configKiosk // { id = 1; };
        contacts   = configKiosk // { id = 2; };
        calendar   = configKiosk // { id = 3; };
        protonmail = configKiosk // { id = 4; };
        gmail      = configKiosk // { id = 5; };
        messenger  = configKiosk // { id = 6; };
        whatsapp   = configKiosk // { id = 7; };
        work       = {
          inherit userChrome userContent extensions;
          settings = settingsMain;
          id       = 8;
        };
        slack      = configKiosk // {
          id = 9;
          settings = settingsKiosk // {
            "general.useragent.override" = "Mozilla/5.0 (Windows NT 10.0; rv:109.0) Gecko/20100101 Firefox/118.0";
          };
        };
        zoom       = configKiosk // { id = 10; };
      };
  }

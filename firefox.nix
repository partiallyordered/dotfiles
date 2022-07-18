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
in
  {
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
      redirector
      skip-redirect
      to-google-translate
      transover
      onetab
    ];
    # Useful: https://ffprofile.com/
    profiles =
      let
        settings = {
          # TODO: would be good if possible to get the "firefoxService" services to open all links
          #       in select-browser. So that if I click a link in say, WhatsApp, it's opened in
          #       select-browser. Perhaps though, it's better to use the Matrix-Element bridge and
          #       ditch the Whatsapp browser app? Consider paying for Element One?
          #       - https://github.com/mautrix/whatsapp
          #       - https://matrix.org/docs/guides/whatsapp-bridging-mautrix-whatsapp
          # TODO: can/should we configure some search engines here?
          #         see: https://wiki.archlinux.org/title/Firefox#Adding_search_engines
          # TODO: https://wiki.archlinux.org/title/Firefox/Tweaks
          # TODO: find setting to stop firefox hassling me about default browser
          # TODO: set default search engine to not-Google

          # TODO: not yet working:
          # https://wiki.archlinux.org/title/Firefox#Dark_themes
          "ui.systemUsesDarkTheme" = 1;
          # TODO: set in Firefox 100
          # "layout.css.prefers-color-scheme.content-override" = 0;

          "browser.shell.checkDefaultBrowser" = false;
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
          "privacy.resistFingerprinting" = true;
          "dom.battery.enabled" = false;
          "dom.event.clipboardevents.enabled" = false;
          "geo.enabled" = false;
          "media.navigator.enabled" = false;
          "network.http.referer.trimmingPolicy" = 2;
          "extensions.activeThemeID" = "firefox-compact-dark@mozilla.org";
          "browser.aboutConfig.showWarning" = false;
          "extensions.pocket.enabled" = false;
        };
        settingsKiosk = settings // {
          "identity.fxaccounts.enabled" = false;
          "privacy.clearOnShutdown.cache" = true;
          "privacy.clearOnShutdown.cookies" = true;
          "privacy.clearOnShutdown.downloads" = true;
          "privacy.clearOnShutdown.formdata" = true;
          "privacy.clearOnShutdown.history" = true; # TODO: does not seem to be working
          "privacy.clearOnShutdown.openWindows" = true;
          "privacy.clearOnShutdown.offlineApps" = true;
          # don't want to have to log in to this stuff every time
          "privacy.clearOnShutdown.siteSettings" = false;
          "privacy.sanitize.sanitizeOnShutdown" = false;
          "privacy.clearOnShutdown.sessions" = false;
        };
        # https://www.userchrome.org/
        # https://www.userchrome.org/find-user-style-recipes.html
        # https://firefox-source-docs.mozilla.org/devtools-user/browser_toolbox/index.html
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
          inherit settings userChrome userContent;
          id = 0;
        };
        app        = configKiosk // { id = 1; };
        contacts   = configKiosk // { id = 2; };
        calendar   = configKiosk // { id = 3; };
        protonmail = configKiosk // { id = 4; };
        gmail      = configKiosk // { id = 5; };
        messenger  = configKiosk // { id = 6; };
        whatsapp   = configKiosk // { id = 7; };
        ohalo      = {
          inherit settings userChrome userContent;
          id = 8;
        };
      };
  }

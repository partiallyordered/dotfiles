--
-- xmonad example config file for xmonad-0.9
--
-- A template showing all available configuration hooks,
-- and how to override the defaults in your own xmonad.hs conf file.
--
-- Normally, you'd only override those defaults you care about.
--
-- NOTE: Those updating from earlier xmonad versions, who use
-- EwmhDesktops, safeSpawn, WindowGo, or the simple-status-bar
-- setup functions (dzen, xmobar) probably need to change
-- xmonad.hs, please see the notes below, or the following
-- link for more details:
--
-- http://www.haskell.org/haskellwiki/Xmonad/Notable_changes_since_0.8
--
--
-- TODO:
--  - Toggle window titles
--  - Check whether xcompmgr is present and set window border width to 1 if it
--    is not.
--  - some default layouts on VM workspaces, pidgin workspace, firefox workspace
--  - more controlled layout switching; i.e. <M-S-numpad1> through <M-S-numpadn> for layouts
--  - A submenu alt+space, [1|2|3|4|..] to select layout
--  - Select layout by name. See the 'description' method on LayoutClass here:
--    https://hackage.haskell.org/package/xmonad-bluetilebranch-0.9.1.4/docs/XMonad-Core.html
--    That combined with dmenu would probably allow selection of layouts by name
--  - SmartBorders
--  - ephemeral workspace names - pop up a teensy menu to name a workspace, then
--    pop up a menu to select one by name (dmenu?)
--  - ephemeral window names - pop up a teensy menu to name a window, then pop up
--    a menu to select one by name (dmenu?) (or by number?)
--  - searchable window names: create a list of all windows currently open, and
--    relevant properties (_NET_WM_NAME, WM_NAME, WM_CLASS, WM_WINDOW_ROLE etc.) then
--    provide them to dmenu and switch to the workspace they're on and focus them
--  - toggling of window titles and borders
--  - Check this out:
--    http://xmonad.org/xmonad-docs/xmonad-contrib/XMonad-Layout-DecorationMadness.html
--  - Temporary workspaces? Perhaps in a different workspace namespace or
--    something? (How would I get back to it?)
--  - Some sort of command-watcher; i.e. plug a command-line command into xmonad and display its
--    result/progress/output somewhere on-screen
--  - Process killing dmenu (or possibly an xmonad-specific alternative)
--  - Tool to duplicate a terminal (or possibly any given window) by checking the directory open in
--    the current terminal. Use case: want to open new terminal at directory of existing terminal
--    in minimal keystrokes. If inside another prog (e.g. vim) open new terminal at same directory
--    as current terminal regardless. Probably has to be shell/terminal-dependent. (Can we get
--    working directory of open shell session?)
--  - Macros? See VIMonad? And: http://lynnard.me/blog/2013/11/05/building-a-vim-like-xmonad-prompt-task-groups-topical-workspaces-float-styles-and-more/
--  - When opening a file in vim that's already open in xmonad, jump to that window/workspace (this
--    is probably a zshrc thing, but it's in these todos anyway)
--  - GridSelect with overlay keys like easymotion
--  - Make and experiment with 'focus' functionality, which fades or blacks out all screens except
--    the focused one
--  - Extension to display pstree overlay on each window, where the root is the top pid for that
--    window
--  - Loud overlay when I press caps lock? Or just remap capslock.. (Can I do that with xmonad?
--    kmonad?)
--  - Opposite of current <M-b> - send C-q or C-S-q to various applications
--  - World clock workspace
--  - Command to create a new throw-away chromium instance. I.e. in private browsing mode, with no
--    history, no profile, etc.
--  - Toggle window titles on and off; remove layouts that are only different by window title
--  - Ability to send a window to the same workspace as another window, where the target
--    window/workspace is selected with dmenu

import XMonad
import Data.Maybe (fromMaybe)
import Data.Monoid
import Data.List
import qualified Data.Map.Strict as StrictMap (fromList)
import System.Exit
import XMonad.Layout.NoBorders
import XMonad.Actions.Warp (banish, Corner (UpperLeft))
import XMonad.Actions.WindowGo
import XMonad.Actions.CycleWorkspaceByScreen (cycleWorkspaceOnCurrentScreen)
import XMonad.Actions.CycleWS (nextWS, prevWS)
import XMonad.Actions.Search
import XMonad.Actions.Navigation2D
import XMonad.Actions.FindEmptyWorkspace (viewEmptyWorkspace)
import XMonad.Hooks.FadeInactive
import XMonad.Hooks.FadeWindows
import XMonad.Hooks.ManageDocks
import XMonad.Layout.Grid
import XMonad.Layout.NoFrillsDecoration
import XMonad.Layout.Spiral
import XMonad.Actions.WindowBringer
import XMonad.Actions.TagWindows
import XMonad.Util.XUtils
import XMonad.Util.Font
import Control.Monad
import XMonad.Actions.EasyMotion (selectWindow, EasyMotionConfig(..), ChordKeys( PerScreenKeys ))
import XMonad.Hooks.EwmhDesktops
import XMonad.Actions.FocusNth (swapNth, focusNth)
import XMonad.Hooks.WorkspaceHistory (workspaceHistoryHook)

import qualified XMonad.Prompt                as P
import qualified XMonad.Actions.Submap        as SM
import qualified XMonad.Actions.Search        as S
import qualified XMonad.StackSet              as W
import qualified Data.Map                     as M
import qualified XMonad.Util.WindowProperties as WP

-- The preferred terminal program, which is used in a binding below and by
-- certain contrib modules.
myTerminal      = "alacritty"

-- Whether focus follows the mouse pointer.
myFocusFollowsMouse :: Bool
myFocusFollowsMouse = True

-- Width of the window border in pixels.
--
myBorderWidth   = 1

-- modMask lets you specify which modkey you want to use. The default
-- is mod1Mask ("left alt").  You may also consider using mod3Mask
-- ("right alt"), which does not conflict with emacs keybindings. The
-- "windows key" is usually mod4Mask.
--
myModMask       = mod1Mask

-- NOTE: from 0.9.1 on numlock mask is set automatically. The numlockMask
-- setting should be removed from configs.
--
-- You can safely remove this even on earlier xmonad versions unless you
-- need to set it to something other than the default mod2Mask, (e.g. OSX).
--
-- The mask for the numlock key. Numlock status is "masked" from the
-- current modifier status, so the keybindings will work with numlock on or
-- off. You may need to change this on some systems.
--
-- You can find the numlock modifier by running "xmodmap" and looking for a
-- modifier with Num_Lock bound to it:
--
-- > $ xmodmap | grep Num
-- > mod2        Num_Lock (0x4d)
--
-- Set numlockMask = 0 if you don't have a numlock key, or want to treat
-- numlock status separately.
--
-- myNumlockMask   = mod2Mask -- deprecated in xmonad-0.9.1
------------------------------------------------------------

-- The default number of workspaces (virtual screens) and their names.
-- By default we use numeric strings, but any string may be used as a
-- workspace name. The number of workspaces is determined by the length
-- of this list.
--
-- A tagging example:
--
-- > workspaces = ["web", "irc", "code" ] ++ map show [4..9]
--
-- Unfortunately, we need to manually keep this up to date with the polybar workspaces config
myWorkspaces    =
  [ "`", "1", "2", "3", "4", "5", "6", "7", "8", "9", "0", "-", "=", "BS", "INS" , "HOME", "PGUP"
  , "whatsapp", "gmail", "protonmail", "calendar", "contacts", "signal", "spotify", "zeal"
  , "chromium"
  ]

-- Border colors for unfocused and focused windows, respectively.
--
-- myNormalBorderColor  = "#dddddd"
-- myFocusedBorderColor = "#ff0000"
myNormalBorderColor  = "#002b36" -- Solarized dark background colour
myFocusedBorderColor = "#657b83" -- Solarized dark foreground colour

-- Search engines
ddg = searchEngine "DuckDuckGo" "https://duckduckgo.com/?q="
-- Search engine map
searchEngineMap method = M.fromList
    [ ((0, xK_g), method S.google)
    , ((0, xK_w), method S.wikipedia)
    , ((0, xK_m), method S.maps)
    , ((0, xK_d), method ddg)
    ]
-- Warning: This gotoWindow function assumes you made your workspaces
-- with the 'withScreens' function from XMonad.Layout.IndependentScreens
-- gotoWindow :: Window -> WindowSet -> WindowSet
-- gotoWindow window ws = case S.findTag window ws of
--                            Just i -> viewOnScreen (screenIdFromTag i) i ws
--                            Nothing -> ws
--     where
--         screenIdFromTag :: WorkspaceId -> ScreenId
--         screenIdFromTag = S . read . takeWhile (/= '_')

-- layoutMap

-- searchAndGoTo = do
--     SM.submap $ searchEngineMap $ S.promptSearch P.defaultXPConfig
--     runOrRaiseNext "firefox" (stringProperty "WM_WINDOW_ROLE" =? "browser")

-- Check whether a query passes. If it does, do nothing. If it does not, run
-- "spawn spawncmd"
checkAndSpawn :: XMonad.Query Bool -> String -> X ()
checkAndSpawn query spawncmd =
    ifWindows query (\w -> return ()) (spawn spawncmd)

emConf :: EasyMotionConfig
emConf = def {
               sKeys = PerScreenKeys $ StrictMap.fromList [(0, [xK_a, xK_s, xK_d, xK_f]), (1, [xK_h, xK_j, xK_k, xK_l])]
             , maxChordLen = 1
             }

------------------------------------------------------------------------
-- Key bindings. Add, modify or remove key bindings here.
-- Discussion of key codes in xmonad:
-- https://old.reddit.com/r/xmonad/comments/638pbj/how_to_find_out_any_keysym_instantly/
-- In nixos, finding a key:
--   nix-shell -p xorg.xev --run "xev -event keyboard"
--
myKeys conf@XConfig {XMonad.modMask = modm} = M.fromList $

    -- launch a terminal
    [ ((modm               , xK_Return), spawn $ XMonad.terminal conf)
    -- launch a terminal in an empty workspace
    , ((modm .|. shiftMask , xK_Return), viewEmptyWorkspace >> spawn (XMonad.terminal conf))

    -- launch ephemeral vim
    , ((modm              , xK_v     ), spawn (XMonad.terminal conf ++ " -e $EDITOR"))
    -- launch ephemeral vim in an empty workspace
    , ((modm .|. shiftMask, xK_v     ), viewEmptyWorkspace >> spawn (XMonad.terminal conf ++ " -e $EDITOR"))

    -- find an empty workspace
    , ((modm,               xK_period), viewEmptyWorkspace)

    -- window tagging (m-a, 'a' for 'annotate')
    , ((modm,               xK_a     ), tagPrompt def (withFocused . addTag))
    -- , ((modm .|. shiftMask, xK_a     ), tagPrompt defaultXPConfig (`withTaggedGlobalP` gotoWindow))
    -- , ((modm .|. shiftMask, xK_a     ), tagPrompt defaultXPConfig (\s -> withTaggedGlobalP s shiftHere))
    -- , ((modm .|. shiftMask, xK_a     ), tagPrompt defaultXPConfig (\s -> shiftToScreen s))
    , ((modm,               xK_f     ), selectWindow emConf >>= flip whenJust (windows . W.focusWindow))

    -- suspend
    , ((modm .|. shiftMask, xK_s     ), spawn "systemctl suspend")

    -- lock screen with Win+L (lock buttons on keyboards send Win+L)
    , ((mod4Mask,           xK_l     ), spawn "loginctl lock-session $XDG_SESSION_ID")

    -- PrintScreen button to start flameshot
    , ((noModMask,          xK_Print ), spawn "flameshot gui --path /home/msk/screenshots/")

    -- Cycle workspaces
    , ((modm,               xK_l     ), nextWS)
    , ((modm,               xK_h     ), prevWS)

    -- cycle through recent workspaces in recently-used order
    -- documentation for this module is much better in 0.17.0.9 than it is in 0.17
    -- https://xmonad.github.io/xmonad-docs/xmonad-contrib-0.17.0.9/XMonad-Actions-CycleWorkspaceByScreen.html
    , ((modm,               xK_Tab   ), cycleWorkspaceOnCurrentScreen [xK_Alt_L] xK_Tab xK_p)

    -- launch application runner
    , ((modm,               xK_p     ), spawn "rofi -show run")

    -- close focused window
    , ((modm .|. shiftMask, xK_c     ), kill)

     -- Rotate through the available layout algorithms
    , ((modm,               xK_space ), sendMessage NextLayout)

    --  Reset the layouts on the current workspace to default
    , ((modm .|. shiftMask, xK_space ), setLayout $ XMonad.layoutHook conf)

    -- Resize viewed windows to the correct size
    , ((modm,               xK_n     ), refresh)

    -- Turn volume up 10%
    , ((modm,               xK_KP_Add ), spawn "pactl set-sink-volume $(pactl list short | grep RUNNING | cut -f1) +10%")
    -- XF86AudioRaiseVolume
    , ((noModMask,          0x1008ff13 ), spawn "pactl set-sink-volume $(pactl list short | grep RUNNING | cut -f1) +10%")

    -- Previous track
    -- XF86AudioPrev
    , ((noModMask,          0x1008ff16 ), spawn "dbus-send --print-reply --dest=org.mpris.MediaPlayer2.spotify /org/mpris/MediaPlayer2 org.mpris.MediaPlayer2.Player.Previous")

    -- Play/pause
    -- XF86AudioPlay
    , ((noModMask,          0x1008ff14 ), spawn "dbus-send --print-reply --dest=org.mpris.MediaPlayer2.spotify /org/mpris/MediaPlayer2 org.mpris.MediaPlayer2.Player.PlayPause")
    , ((modm,               xK_KP_Begin ), spawn "dbus-send --print-reply --dest=org.mpris.MediaPlayer2.spotify /org/mpris/MediaPlayer2 org.mpris.MediaPlayer2.Player.PlayPause")

    -- Next track
    -- XF86AudioNext
    , ((noModMask,          0x1008ff17 ), spawn "dbus-send --print-reply --dest=org.mpris.MediaPlayer2.spotify /org/mpris/MediaPlayer2 org.mpris.MediaPlayer2.Player.Next")

    -- Turn volume down 10%
    , ((modm,               xK_KP_Subtract ), spawn "pactl set-sink-volume $(pactl list short | grep RUNNING | cut -f1) -10%")
    -- XF86AudioLowerVolume
    , ((noModMask,          0x1008ff11 ), spawn "pactl set-sink-volume $(pactl list short | grep RUNNING | cut -f1) -10%")

    -- Toggle mute
    , ((modm,               xK_KP_Insert ), spawn "pactl set-sink-mute $(pactl list short | grep RUNNING | cut -f1) toggle")
    , ((noModMask,          0x1008ff12 ), spawn "pactl set-sink-mute $(pactl list short | grep RUNNING | cut -f1) toggle")

    -- Move focus to the next window
    , ((modm,               xK_j     ), windows W.focusDown)

    -- Move focus to the previous window
    , ((modm,               xK_k     ), windows W.focusUp  )

    -- Swap windows
    , ((modm,               xK_s     ), do
                                          win      <- selectWindow emConf
                                          stack    <- gets $ W.index . windowset
                                          let match = find ((win ==) . Just . fst) $ zip stack [0 ..]
                                          whenJust match $ (\i -> swapNth i >> focusNth i) . snd)

    -- Push window back into tiling
    , ((modm,               xK_t     ), withFocused $ windows . W.sink)

    -- Toggle the status bar gap
    -- Use this binding with avoidStruts from Hooks.ManageDocks.
    -- See also the statusBar function from Hooks.DynamicLog.
    --
    -- , ((modm              , xK_b     ), sendMessage ToggleStruts)

    -- Window bringer
    -- , ((modm              , xK_f     ), windowPrompt def Goto wsWindows)
    {-  TODO: this could be xK_/ when xK_f is easymotion-like -}
    , ((modm              , xK_o     ), spawn "rofi -theme-str 'window {width: 45%;}' -show window")
    -- , ((modm              , xK_o     ), gotoMenuConfig windowBringerConfig)

    -- Quit xmonad
    -- Never want to do this- can use a different VT if necessary
    -- , ((modm .|. shiftMask, xK_q     ), io (exitWith ExitSuccess))

    -- Restart xmonad
    , ((modm              , xK_q     ), spawn "xmonad --restart")
    ]
    ++

    --
    -- mod-[1..9], Switch to workspace N
    --
    -- mod-[1..9], Switch to workspace N
    -- mod-shift-[1..9], Move client to workspace N
    --
    [((m .|. modm, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_grave, xK_1, xK_2, xK_3, xK_4, xK_5, xK_6, xK_7, xK_8, xK_9, xK_0, xK_minus, xK_equal, xK_BackSpace, xK_Insert, xK_Home, xK_Page_Up]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
    ++

    --
    -- mod-{w,e,r}, Switch to physical/Xinerama screens 1, 2, or 3
    -- mod-shift-{w,e,r}, Move client to screen 1, 2, or 3
    --
    [((m .|. modm, key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]


------------------------------------------------------------------------
-- Mouse bindings: default actions bound to mouse events
--
myMouseBindings (XConfig {XMonad.modMask = modm}) = M.fromList $

    -- mod-button1, Set the window to floating mode and move by dragging
    [ ((modm, button1), (\w -> focus w >> mouseMoveWindow w
                                       >> windows W.shiftMaster))

    -- mod-button2, Raise the window to the top of the stack
    , ((modm, button2), (\w -> focus w >> windows W.shiftMaster))

    -- mod-button3, Set the window to floating mode and resize by dragging
    , ((modm, button3), (\w -> focus w >> mouseResizeWindow w
                                       >> windows W.shiftMaster))

    -- you may also bind events to the mouse scroll wheel (button4 and button5)
    ]

------------------------------------------------------------------------
-- Layouts:

-- You can specify and transform your layouts by modifying these values.
-- If you change layout bindings be sure to use 'mod-shift-space' after
-- restarting (with 'mod-q') to reset your layout state to the new
-- defaults, as xmonad preserves your old layout settings by default.
--
-- The available layouts.  Note that each layout is separated by |||,
-- which denotes layout choice.
--
myLayout = avoidStruts $ noBorders tiled ||| Mirror (noBorders tiled) ||| noBorders Full ||| GridRatio (16/10)
  where
    -- default tiling algorithm partitions the screen into two panes
    tiled   = Tall nmaster delta ratio

    -- The default number of windows in the master pane
    nmaster = 1

    golden  = toRational (2/(1+sqrt 5::Double))

    -- Default proportion of screen occupied by master pane
    ratio   = 1/2

    -- Percent of screen to increment by when resizing panes
    delta   = 3/100

------------------------------------------------------------------------
-- Window rules:

-- Execute arbitrary actions and WindowSet manipulations when managing
-- a new window. You can use this to, for example, always float a
-- particular program, or have a client always appear on a particular
-- workspace.
--
-- To find the property name associated with a program, use
-- > xprop | grep WM_CLASS
-- and click on the client you're interested in.
--
-- To match on the WM_NAME, you can use 'title' in the same way that
-- 'className' and 'resource' are used below.
--
myManageHook = manageDocks <+> composeAll
    [ className =? "MPlayer"                      --> doFloat
    , className =? "Gimp"                         --> doFloat
    , className =? "Vmplayer"                     --> doFloat
    , resource  =? "desktop_window"               --> doIgnore
    , resource  =? "kdesktop"                     --> doIgnore
    , className =? "Navigator"                    --> doShift "`"
    , className =? "firefox"                      --> doShift "`"
    , className =? "Spotify"                      --> doShift "spotify"
    , className =? "spotify"                      --> doShift "spotify"
    , className =? "Signal"                       --> doShift "signal"
    , className =? "whatsapp"                     --> doShift "whatsapp"
    , className =? "protonmail"                   --> doShift "protonmail"
    , className =? "gmail"                        --> doShift "gmail"
    , className =? "calendar"                     --> doShift "calendar"
    , className =? "contacts"                     --> doShift "contacts"
    , className =? "zeal"                         --> doShift "zeal"
    , className =? "chromium-browser"             --> doShift "chromium"
    , className =? "Chromium-browser"             --> doShift "chromium"
    ]

    -- runOrRaiseNext "firefox" (stringProperty "WM_WINDOW_ROLE" =? "browser")
------------------------------------------------------------------------
-- Event handling

-- Defines a custom handler function for X Events. The function should
-- return (All True) if the default handler is to be run afterwards. To
-- combine event hooks use mappend or mconcat from Data.Monoid.
--
-- myEventHook = fadeWindowsEventHook
myEventHook = mempty

------------------------------------------------------------------------
-- Status bars and logging

-- Perform an arbitrary action on each internal state change or X event.
-- See the 'XMonad.Hooks.DynamicLog' extension for examples.
--
myLogHook =  workspaceHistoryHook >> fadeInactiveLogHook fadeAmount where fadeAmount = 0.92
-- myLogHook = fadeWindowsLogHook $ composeAll [isUnfocused --> transparency 0.2
--                                             ,                transparency 0.1
--                                             ]

------------------------------------------------------------------------
-- Startup hook

-- Perform an arbitrary action each time xmonad starts or is restarted
-- with mod-q.  Used by, e.g., XMonad.Layout.PerWorkspace to initialize
-- per-workspace layout choices.
--
-- By default, do nothing.
--
myStartupHook = mempty

------------------------------------------------------------------------
-- Now run xmonad with all the defaults we set up.

-- Run xmonad with the settings you specify. No need to modify this.
--
main = xmonad $ ewmh $ docks defaults

-- A structure containing your configuration settings, overriding
-- fields in the default config. Any you don't override, will
-- use the defaults defined in xmonad/XMonad/Config.hs
--
-- No need to modify this.
--
defaults = defaultConfig {
      -- simple stuff
        terminal           = myTerminal,
        focusFollowsMouse  = myFocusFollowsMouse,
        borderWidth        = myBorderWidth,
        modMask            = myModMask,
        -- numlockMask deprecated in 0.9.1
        -- numlockMask        = myNumlockMask,
        workspaces         = myWorkspaces,
        normalBorderColor  = myNormalBorderColor,
        focusedBorderColor = myFocusedBorderColor,

      -- key bindings
        keys               = myKeys,
        mouseBindings      = myMouseBindings,

      -- hooks, layouts
        layoutHook         = myLayout,
        manageHook         = myManageHook,
        handleEventHook    = myEventHook,
        logHook            = myLogHook,
        startupHook        = myStartupHook
    }

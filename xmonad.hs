-- TODO:
--  - more controlled layout switching; i.e. <M-S-numpad1> through <M-S-numpadn> for layouts
--    - A submenu alt+shift+space, [1|2|3|4|..] to select layout
--    - Select layout by name. See the 'description' method on LayoutClass here:
--      https://hackage.haskell.org/package/xmonad-bluetilebranch-0.9.1.4/docs/XMonad-Core.html
--      That combined with dmenu would probably allow selection of layouts by name
--    - GridSelect with overlay keys like easymotion
--  - ephemeral workspace names - pop up a teensy menu to name a workspace, then
--    pop up a menu to select one by name (dmenu?)
--  - ephemeral window names - pop up a teensy menu to name a window, then pop up
--    a menu to select one by name (dmenu?) (or by number?)
--  - Temporary workspaces? Perhaps in a different workspace namespace or
--    something? (How would I get back to it?)
--  - Some sort of command-watcher; i.e. plug a command-line command into xmonad and display its
--    result/progress/output somewhere on-screen (use pueue + notify-send)
--  - Tool to duplicate a terminal (or possibly any given window) by checking the directory open in
--    the current terminal. Use case: want to open new terminal at directory of existing terminal
--    in minimal keystrokes. If inside another prog (e.g. vim) open new terminal at same directory
--    as current terminal regardless. Probably has to be shell/terminal-dependent. (Can we get
--    working directory of open shell session?)
--  - Macros? See VIMonad? And: http://lynnard.me/blog/2013/11/05/building-a-vim-like-xmonad-prompt-task-groups-topical-workspaces-float-styles-and-more/
--  - Make and experiment with 'focus' functionality, which fades or blacks out all windows except
--    the focused one.
--  - display pstree overlay on each window, where the root is the top pid for that window
--  - World clock overlay/popup- perhaps a surf instance or two containing time.is when clicking
--    the polybar clock?
--  - Ability to send a window to the same workspace as another window, where the target window is
--    selected with dmenu. I.e. send a terminal window to the same workspace as firefox. Or, more
--    usefully, send a terminal window to the same workspace as another terminal window where the
--    same work is occurring (use some sort of dynamic workspace tags?)
--  - With polybar and m-/ workspaces 1-9 might be a little less important- consider removing them
--    and using dynamic workspaces with names (or even without names, it's good being able to m-1
--    to m-0 to get to them quickly) instead. Consider dynamic workspace tags that don't affect the
--    actual workspace name- those could be searchable using dmenu without removing the ability to
--    quickly switch workspaces using m-` through m-pgup. The tag of the current workspace _could_
--    be displayed somewhere (e.g. polybar).
--  - Shuffle all windows down into empty workspaces that come before. E.g. if workspace 4 is
--    empty, shuffle the contents of workspace 5 to workspace 4, workspace 6 to workspace 5, etc.
--    XMonad.Actions.WithAll could be useful here.
--  - Replace m-j and m-k with m-h and m-l (and 2dnavigation, or whatever it was before that let
--    focus move left and right with m-h and m-l)
--    - Maaaaybe. Because I find I use m-h and m-l to move between windows more than I use m-j and
--      m-k.
--    - m-h and m-l would have to function like m-j and m-k on certain workspaces, like browser
--      workspaces
--  - There's a module to display your keyboard mappings- could be kinda nice, if it's easy to set
--    up
--  - look at dmesg integration
--  - is it possible to set rofi up so I can press c-enter to open something in a new workspace,
--    and go to that workspace, and c-s-enter to open something in terminal in a new workspace, and
--    go to that workspace
--  - m-h and m-l could go to next occupied workspace, instead of next workspace
--    - or workspaces could be created on-demand..
--  - actions using
--    - https://hackage.haskell.org/package/xmonad-contrib-0.17.0/docs/XMonad-Actions-WindowMenu.html
--    - https://hackage.haskell.org/package/xmonad-contrib-0.17.0/docs/XMonad-Actions-GridSelect.html

{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, TypeSynonymInstances #-}

import XMonad
import Data.Maybe (fromMaybe)
import Data.Monoid
import Data.Function ((&))
import Data.List
import qualified Data.Map.Strict as StrictMap (fromList, lookup)
import System.Exit
import XMonad.Layout.NoBorders
import XMonad.Actions.WindowGo
import XMonad.Actions.CycleWorkspaceByScreen (cycleWorkspaceOnCurrentScreen)
import XMonad.Actions.CycleWS (nextWS, prevWS, shiftToPrev, shiftToNext)
import XMonad.Actions.Search
import XMonad.Actions.Navigation2D
import XMonad.Actions.FindEmptyWorkspace (viewEmptyWorkspace)
import XMonad.Hooks.FadeInactive
import XMonad.Hooks.FadeWindows
import XMonad.Hooks.ManageDocks
import XMonad.Actions.TagWindows
-- import XMonad.Util.Types (Rectangle(..))
import XMonad.Util.XUtils
import XMonad.Util.Font
import XMonad.Util.Dmenu (menuArgs)
import Control.Monad
import Control.Lens (element, (^?))
import XMonad.Actions.EasyMotion (selectWindow, EasyMotionConfig(..), ChordKeys( PerScreenKeys ))
import XMonad.Hooks.EwmhDesktops
import XMonad.Actions.FocusNth (swapNth, focusNth)
import XMonad.Hooks.WorkspaceHistory (workspaceHistoryHook)
import XMonad.Actions.UpdatePointer
import XMonad.Layout.PerWorkspace (onWorkspace)
import XMonad.Layout.Spacing (spacingRaw, Border(..))
import XMonad.Layout.MultiToggle (Transformer(..), mkToggle, (??), single, Toggle(..))
import XMonad.Layout.NoFrillsDecoration (noFrillsDeco)

import qualified XMonad.Layout.Decoration     as D
import qualified XMonad.Prompt                as P
import qualified XMonad.Actions.Submap        as SM
import qualified XMonad.Actions.Search        as S
import qualified XMonad.StackSet              as W
import qualified Data.Map                     as M
import qualified XMonad.Util.WindowProperties as WP

-- The preferred terminal program, which is used in a binding below and by
-- certain contrib modules.
myTerminal      = "wezterm"

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
  [ "firefox", "1", "2", "3", "4", "5", "6", "7", "8", "9", "0", "-", "=", "BS", "INS" , "HOME", "PGUP"
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

emConf :: EasyMotionConfig
emConf = def {
               sKeys = PerScreenKeys $ StrictMap.fromList [(0, [xK_a, xK_s, xK_d, xK_f]), (1, [xK_h, xK_j, xK_k, xK_l])]
             , maxChordLen = 1
             }

menuSelectWs :: [WorkspaceId] -> X String
menuSelectWs wss = do
  currWsName <- withWindowSet (pure . W.currentTag)
  let currWsIndex = show <$> elemIndex currWsName wss
  case currWsIndex of
    Just i -> menuArgs "rofi" ["-dmenu", "-i", "-p", "> ", "-no-custom", "-selected-row", i] wss
    -- TODO: this feels wrong. I don't know what I'm doing here. Need to do some reading.
    Nothing -> fail ""

------------------------------------------------------------------------
-- Window decoration
-- Thanks to https://gist.github.com/Avaq/9691fa3f8b6538fb949570884e5ee91e#file-xmonad-hs-L121-L122
-- I've modified this from the original to incorporate window corner radii and window spacing (in a
-- rather hacky manner) on only the bottom decoration. For the other side decorations to function
-- correctly, it'll be necessary to modify them to support this also.
-- TODO: incorporate spacing and corner radii more sensibly.
newtype SideDecoration a = SideDecoration Direction2D deriving (Show, Read)

space = 5 :: Integer

instance Eq a => D.DecorationStyle SideDecoration a where

  shrink b (Rectangle _ _ dw dh) (Rectangle x y w h)
    -- | SideDecoration U <- b = Rectangle x (y + fi dh) w (h - dh)
    -- | SideDecoration R <- b = Rectangle x y (w - dw) h
    | SideDecoration D <- b = Rectangle x y w (h - dh)
    -- | SideDecoration L <- b = Rectangle (x + fi dw) y (w - dw) h

  pureDecoration b dw dh _ st _ (win, Rectangle x y w h)
    | win `elem` W.integrate st && dw < w && dh < h = Just $ case b of
      SideDecoration D -> Rectangle (x + sp + r) (y + fi (h - dh) - sp) (w - sd - fi r * 2) dh
      -- SideDecoration U -> Rectangle x y w dh
      -- SideDecoration R -> Rectangle (x + fi (w - dw)) y dw h
      -- SideDecoration D -> Rectangle x (y + fi (h - dh)) w dh
      -- SideDecoration L -> Rectangle x y dw h
    | otherwise = Nothing
    where
      sp = fi space :: Position
      sd = fi space :: Dimension
      r = 6 -- corner radius

bottomBarTheme :: D.Theme
bottomBarTheme = def
  { D.activeColor         = activeColor
  , D.activeTextColor     = activeColor
  , D.activeBorderColor   = activeColor
  , D.activeBorderWidth   = 0
  , D.inactiveColor       = inactiveColor
  , D.inactiveTextColor   = inactiveColor
  , D.inactiveBorderColor = inactiveColor
  , D.inactiveBorderWidth = 0
  , D.decoWidth           = 5
  , D.decoHeight          = 5 }
    where
      activeColor = "#79d2a6"
      inactiveColor = "#194d33"

bottomBarDecorate :: Eq a => l a -> D.ModifiedLayout (D.Decoration SideDecoration D.DefaultShrinker) l a
bottomBarDecorate = D.decoration D.shrinkText bottomBarTheme (SideDecoration D)

titleBarTheme :: D.Theme
titleBarTheme = def
  { D.activeColor         = activeColor
  , D.activeTextColor     = inactiveColor
  , D.activeBorderColor   = activeColor
  , D.activeBorderWidth   = 0
  , D.inactiveColor       = inactiveColor
  , D.inactiveTextColor   = activeColor
  , D.inactiveBorderColor = inactiveColor
  , D.inactiveBorderWidth = 0
  , D.decoHeight          = 40 }
    where
      activeColor = "#79d2a6"
      inactiveColor = "#194d33"

data NOFRILLSDECO = NOFRILLSDECO deriving (Read, Show, Eq)
instance Transformer NOFRILLSDECO Window where
  transform _ x k = k (noFrillsDeco D.shrinkText titleBarTheme x) (\(D.ModifiedLayout _ x') -> x')

------------------------------------------------------------------------
-- Some terminal helpers
spawnInTerminal c = spawn $ "wezterm start " ++ c
spawnAlacrittyApp c = spawn $ "alacritty -t " ++ c ++ " -e " ++ c

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
    , ((modm              , xK_v     ), spawnInTerminal "$EDITOR")
    -- launch ephemeral vim in an empty workspace
    , ((modm .|. shiftMask, xK_v     ), viewEmptyWorkspace >> spawnAlacrittyApp "$EDITOR")

    -- launch ephemeral lazygit
    , ((modm              , xK_g     ), spawnAlacrittyApp "lazygit")
    -- launch ephemeral lazygit in an empty workspace
    , ((modm .|. shiftMask, xK_g     ), viewEmptyWorkspace >> spawnAlacrittyApp "lazygit")

    -- launch rofi-pass
    , ((modm .|. shiftMask, xK_p     ), spawn "rofi-pass")

    -- find an empty workspace
    , ((modm,               xK_period), viewEmptyWorkspace)

    -- open audio control
    , ((modm,               xK_a     ), spawnAlacrittyApp "ncpamixer")

    -- window tagging (m-a, 'a' for 'annotate')
    -- , ((modm,               xK_a     ), tagPrompt def (withFocused . addTag))
    -- , ((modm .|. shiftMask, xK_a     ), tagPrompt defaultXPConfig (`withTaggedGlobalP` gotoWindow))
    -- , ((modm .|. shiftMask, xK_a     ), tagPrompt defaultXPConfig (\s -> withTaggedGlobalP s shiftHere))
    -- , ((modm .|. shiftMask, xK_a     ), tagPrompt defaultXPConfig (\s -> shiftToScreen s))

    -- Window selection
    , ((modm,               xK_f     ), selectWindow emConf >>= flip whenJust (windows . W.focusWindow))

    -- move window to rofi-selected workspace
    , ((modm .|. shiftMask, xK_slash ), menuSelectWs (XMonad.workspaces conf) >>= (windows . W.shift))
    , ((modm .|. shiftMask .|. controlMask, xK_slash ), do
                                          targetWsName <- menuSelectWs (XMonad.workspaces conf)
                                          windows . W.shift $ targetWsName
                                          windows . W.greedyView $ targetWsName)
    -- jump to rofi-selected workspace
    , ((modm,               xK_slash ), menuSelectWs (XMonad.workspaces conf) >>= (windows . W.greedyView))

    -- lock screen with Win+L (lock buttons on keyboards send Win+L)
    , ((mod4Mask,           xK_l     ), spawn "loginctl lock-session $XDG_SESSION_ID")

    -- PrintScreen button to start flameshot
    , ((noModMask,          xK_Print ), spawn "flameshot gui --path /home/msk/screenshots/")

    -- View prev/next workspace
    , ((modm,               xK_l     ), nextWS)
    , ((modm,               xK_h     ), prevWS)

    -- Shift windows to prev/next workspaces
    , ((modm .|. shiftMask, xK_l     ), shiftToNext)
    , ((modm .|. shiftMask, xK_h     ), shiftToPrev)
    , ((modm .|. shiftMask .|. controlMask, xK_l), shiftToNext >> nextWS)
    , ((modm .|. shiftMask .|. controlMask, xK_h), shiftToPrev >> prevWS)

    -- cycle through recent workspaces in recently-used order
    -- documentation for this module is much better in 0.17.0.9 than it is in 0.17
    -- https://xmonad.github.io/xmonad-docs/xmonad-contrib-0.17.0.9/XMonad-Actions-CycleWorkspaceByScreen.html
    , ((modm,               xK_Tab   ), cycleWorkspaceOnCurrentScreen [xK_Alt_L] xK_Tab xK_p)

    -- launch application runner
    , ((modm,               xK_p     ), spawn "rofi -show run -display-run \"> \"")

     -- Rotate through the available layout algorithms
    , ((modm,               xK_space ), sendMessage NextLayout)

    --  Reset the layouts on the current workspace to default. Useful to update layouts after
    --  config update.
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

    -- Toggle window titles
    , ((modm .|. shiftMask, xK_t     ), sendMessage $ Toggle NOFRILLSDECO)

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
    , ((modm              , xK_o     ), spawn "rofi -theme-str 'window {width: 45%;}' -show window -display-window \"> \"") 
    -- , ((modm              , xK_o     ), gotoMenuConfig windowBringerConfig)

    -- Quit xmonad
    -- Never want to do this- can use a different VT if necessary
    -- , ((modm .|. shiftMask, xK_q     ), io (exitWith ExitSuccess))

    -- Restart xmonad
    , ((modm              , xK_q     ), spawn "xmonad --restart")

    -- Launch default command
    -- TODO: should probably define a single map from workspaces to workspace keys and default
    -- commands
    , ((modm              , xK_c     ), do
      let commands =
            [ "systemctl --user start firefox"
            , XMonad.terminal conf
            , XMonad.terminal conf
            , XMonad.terminal conf
            , XMonad.terminal conf
            , XMonad.terminal conf
            , XMonad.terminal conf
            , XMonad.terminal conf
            , XMonad.terminal conf
            , XMonad.terminal conf
            , XMonad.terminal conf
            , XMonad.terminal conf
            , XMonad.terminal conf
            , XMonad.terminal conf
            , XMonad.terminal conf
            , XMonad.terminal conf
            , XMonad.terminal conf
            , "systemctl --user start whatsapp"
            , "systemctl --user start gmail"
            , "systemctl --user start protonmail"
            , "systemctl --user start calendar"
            , "systemctl --user start contacts"
            , "systemctl --user start signal"
            , "systemctl --user start spotify"
            , "systemctl --user start zeal"
            , "systemctl --user start chromium"
            ]
      currWsName <- withWindowSet (pure . W.currentTag)
      let currWsIndex = elemIndex currWsName (XMonad.workspaces conf)
      whenJust (currWsIndex >>= \x -> commands ^? element x) spawn)
      -- whenJust (currWsIndex >>= (commands ^?) . element) spawn)

    , ((modm .|. shiftMask, xK_c     ),
      withFocused $ \w -> do
        withDisplay $ \d -> do
          let commands = StrictMap.fromList
                [ ("firefox",            "systemctl --user stop firefox"    )
                , ("whatsapp",           "systemctl --user stop whatsapp"   )
                , ("gmail",              "systemctl --user stop gmail"      )
                , ("protonmail",         "systemctl --user stop protonmail" )
                , ("calendar",           "systemctl --user stop calendar"   )
                , ("contacts",           "systemctl --user stop contacts"   )
                , ("Signal",             "systemctl --user stop signal"     )
                , ("Spotify",            "systemctl --user stop spotify"    )
                , ("Zeal",               "systemctl --user stop zeal"       )
                , ("Chromium-browser",   "systemctl --user stop chromium"   )]
          prop <- io $ getTextProperty d w wM_CLASS >>= wcTextPropertyToTextList d
          maybe kill spawn (prop ^? element 1 >>= \cls -> StrictMap.lookup cls commands))

    , ((modm .|. shiftMask .|. controlMask, xK_c     ),
      withFocused $ \w -> do
        withDisplay $ \d -> do
          let commands = StrictMap.fromList
                [ ("firefox",            "systemctl --user restart firefox"    )
                , ("whatsapp",           "systemctl --user restart whatsapp"   )
                , ("gmail",              "systemctl --user restart gmail"      )
                , ("protonmail",         "systemctl --user restart protonmail" )
                , ("calendar",           "systemctl --user restart calendar"   )
                , ("contacts",           "systemctl --user restart contacts"   )
                , ("Signal",             "systemctl --user restart signal"     )
                , ("Spotify",            "systemctl --user restart spotify"    )
                , ("Zeal",               "systemctl --user restart zeal"       )
                , ("Chromium-browser",   "systemctl --user restart chromium"   )]
          prop <- io $ getTextProperty d w wM_CLASS >>= wcTextPropertyToTextList d
          maybe (return ()) spawn (prop ^? element 1 >>= \cls -> StrictMap.lookup cls commands))
    ]
    ++

    --
    -- mod-[1..9], Switch to workspace N
    --
    -- mod-[1..9], Switch to workspace N
    -- mod-shift-[1..9], Move client to workspace N
    -- mod-shift-ctrl-[1..9], Move client to workspace N, switch to workspace N
    --
    -- modm is the standard mod key, i.e. left-alt
    -- k is the workspace key, e.g. grave through pgup
    -- f is the function called on the workspace: W.greedyView to show a workspace, or W.shift to move a window to a workspace
    -- m is the mask, nothing (zero) or shiftMask
    -- i is the workspace identifier
    [((m .|. modm, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_grave, xK_1, xK_2, xK_3, xK_4, xK_5, xK_6, xK_7, xK_8, xK_9, xK_0, xK_minus, xK_equal, xK_BackSpace, xK_Insert, xK_Home, xK_Page_Up]
        , (f, m) <- [ (W.greedyView, 0), (W.shift, shiftMask)]]
    ++
    -- TODO: we seem to need to "take" `windows` twice. Any version of this that doesn't seems to
    -- fail. I appear to misunderstand something here.
    [((m .|. modm, k), windows (W.shift i) >> windows (W.greedyView i))
        | (i, k) <- zip (XMonad.workspaces conf) [xK_grave, xK_1, xK_2, xK_3, xK_4, xK_5, xK_6, xK_7, xK_8, xK_9, xK_0, xK_minus, xK_equal, xK_BackSpace, xK_Insert, xK_Home, xK_Page_Up]
        , m <- [shiftMask .|. controlMask]]
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
myMouseBindings (XConfig {XMonad.modMask = modm}) = M.fromList

    -- mod-button1, Set the window to floating mode and move by dragging
    [ ((modm, button1), \w -> focus w >> mouseMoveWindow w >> windows W.shiftMaster)

    -- mod-button2, Raise the window to the top of the stack
    , ((modm, button2), \w -> focus w >> windows W.shiftMaster)

    -- mod-button3, Set the window to floating mode and resize by dragging
    , ((modm, button3), \w -> focus w >> mouseResizeWindow w >> windows W.shiftMaster)

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

myLayout = standardLayout
         & onWorkspace "firefox" ffLayout
         & spacing
         & mkToggle (single NOFRILLSDECO)
         & smartBorders -- removes borders when something is full screen- noBorders does not
         & noBorders
         & avoidStruts
  where
    standardLayout = tiledLayout ||| Full
    ffLayout       = Full
    tiledLayout    = Tall nmaster delta ratio
                   & bottomBarDecorate
    -- The default number of windows in the master pane
    nmaster = 1
    -- Default proportion of screen occupied by master pane
    ratio   = 1/2
    -- Percent of screen to increment by when resizing panes
    delta   = 3/100
    -- Equal gaps between windows
    -- https://wiki.archlinux.org/title/Xmonad#Equally_sized_gaps_between_windows
    -- Removed the top screen border because polybar has its own padding.
    spacing = spacingRaw False (Border 0 0 space 0) True (Border 0 space 0 space) True

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
    , className =? "Navigator"                    --> doShift "firefox"
    , className =? "firefox"                      --> doShift "firefox"
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

------------------------------------------------------------------------
-- Event handling

-- Defines a custom handler function for X Events. The function should
-- return (All True) if the default handler is to be run afterwards. To
-- combine event hooks use mappend or mconcat from Data.Monoid.
--
myEventHook = mempty

------------------------------------------------------------------------
-- Status bars and logging

-- Perform an arbitrary action on each internal state change or X event.
-- See the 'XMonad.Hooks.DynamicLog' extension for examples.
--
myLogHook = updatePointer (0.5, 0.5) (0, 0) >> workspaceHistoryHook >> fadeInactiveLogHook fadeAmount
  where fadeAmount = 0.92

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
-- Run xmonad with the settings you specify. No need to modify this.
--
main = xmonad $ ewmh $ ewmhFullscreen $ docks defaults

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

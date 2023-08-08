-- TODO:
--  - Use XMonad.Layout.WorkspaceDir - but is there some way to have this set by terminals/shells in
--    the workspace whenever they change working directory? Perhaps they could call xmonadctl when
--    the chpwd function is invoked to change directory.
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
--    - see _NET_WM_PID
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
--    - https://hackage.haskell.org/package/xmonad-contrib-0.17.0/docs/XMonad-Actions-TreeSelect.html
--  - when moving to a named workspace, `systemctl start` the desired application automatically?
--    (Some of the workspace topic, project etc. xmonad-contrib modules might be useful for this).
--  - X selection prompt menu to use the current X selection (text) to:
--    - translate in crow
--    - search in browser
--  - Where appropriate, replace rofi usage with XMonad.Prompt
--  - Key bindings to
--    - move all windows in a workspace to another workspace
--    - "collapse" non-empty workspaces into empty workspaces. i.e. if workspaces 1 through 4 are
--      empty, and workspaces 5 and 6 are empty, move the contents of workspace 5 to workspace 1,
--      and workspace 6 to workspace 2.
--      - when the keybinding is used, only collapse the current workspace and earlier. In the
--        previous example, if workspace 7 was also non-empty, but the keybinding was used at
--        workspace 6, collapse only workspaces 6 and earlier, and leave workspaces 7 and later
--        unchanged.
--  - "Jump to zoom meeting"
--    - Probably requires preventing zoom from popping into miniature window
--      - Zoom settings?
--      - Don't send the window unfocus event/message/whatever?
--        - This is better than zoom settings, because it doesn't require zoom to be logged in.
--  - Command to freeze/unfreeze (i.e. systemctl freeze) all processes in a workspace (because a
--    window manager workspace normally corresponds fairly well to a set of project work). The
--    desire for this is largely driven by development environments that consume a lot of
--    resources.
--    - is it possible to make it very clear that the workspace is frozen? with some overlay window
--      or something?
--  - Serialize workspace working directories to disk and reload on XMonad restart?
--    - What about open terminals and their state? Can we open terminals with systemd-run and then
--      systemctl freeze and serialize them?
--      - Is this just hibernating? Should I just hibernate?
--      - Can we prevent the binaries the serialized terminal instances are using from
--        disappearing? This includes the terminal emulator and shell at least, but also any
--        subshells. What about things that are running live in the terminal? Presumably systemctl
--        freeze serializes all of this, actually- I think its images can be transferred between
--        machines.
--  - Try a "modal" xmonad. Perhaps with key sequences or something. E.g. an "activation" key M-X
--    or something, then submaps.

{-# LANGUAGE FlexibleContexts, MultiParamTypeClasses, FlexibleInstances, TypeSynonymInstances #-}

import XMonad
import Data.Maybe (fromMaybe)
import Data.Monoid
import Data.Function ((&))
import Data.List
import System.Exit
import XMonad.Layout.NoBorders
import XMonad.Actions.WindowGo
import XMonad.Actions.CycleWorkspaceByScreen (cycleWorkspaceOnCurrentScreen)
import XMonad.Actions.CycleWS (nextWS, prevWS)
import XMonad.Actions.Search
import XMonad.Actions.Navigation2D
import XMonad.Actions.FindEmptyWorkspace (viewEmptyWorkspace)
import XMonad.Hooks.FadeInactive
import XMonad.Hooks.FadeWindows
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ServerMode
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
import XMonad.Hooks.ManageHelpers (doFocus, (/=?))
import XMonad.Hooks.WindowSwallowing
import qualified XMonad.Hooks.InsertPosition as IP
import XMonad.Hooks.WorkspaceHistory (workspaceHistoryHook)
import XMonad.Actions.UpdatePointer
import XMonad.Layout.PerWorkspace (onWorkspace)
import XMonad.Layout.Spacing (spacingRaw, Border(..), Spacing(..))
import XMonad.Layout.NoFrillsDecoration (noFrillsDeco)
import XMonad.Layout.MultiToggle.Instances (StdTransformers(MIRROR))
import XMonad.Layout.WorkspaceDir
import XMonad.Prompt.Layout (layoutPrompt)
import XMonad.Prompt.XMonad (xmonadPrompt)
import XMonad.Prompt.FuzzyMatch (fuzzyMatch, fuzzySort)
import XMonad.Operations (setLayout)
import XMonad.Prompt.Workspace (Wor(..), workspacePrompt)
import XMonad.Prompt.Window (windowPrompt, WindowPrompt(Goto, Bring), allWindows)
import XMonad.Actions.SwapWorkspaces (swapWithCurrent, swapTo, Direction1D(..))

import XMonad.Layout.Cross (simpleCross)
import XMonad.Layout.Dishes (Dishes(..))
import XMonad.Layout.Grid (Grid(..))
import XMonad.Layout.BinarySpacePartition (emptyBSP)
import XMonad.Layout.Accordion (Accordion(..))
import XMonad.Layout.BinaryColumn (BinaryColumn(..))
import XMonad.Layout.MultiColumns (multiCol)
import XMonad.Layout.MultiDishes (MultiDishes(..))
import XMonad.Layout.OneBig (OneBig(..))
import XMonad.Layout.ResizableThreeColumns (ResizableThreeCol(..))
import XMonad.Layout.ResizableTile (ResizableTall(..))
import XMonad.Layout.Roledex (Roledex(..))
import XMonad.Layout.Spiral (spiral)
import XMonad.Layout.StateFull
import XMonad.Layout.Tabbed (tabbedAlways, tabbedLeftAlways)
import XMonad.Layout.TallMastersCombo (tmsCombineTwoDefault)

import qualified XMonad.Hooks.Focus           as FH
import qualified XMonad.Layout.MultiToggle    as MT
import qualified Data.Map.Strict              as SM
import qualified XMonad.Layout.Decoration     as D
import qualified XMonad.Prompt                as P
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
-- TODO: this could perhaps be managed with nix
myWorkspaces =
  [ "firefox", "1", "2", "3", "4", "5", "6", "7", "8", "9", "0", "-", "=", "BS", "INS" , "HOME", "PGUP"
  , "zoom", "whatsapp", "gmail", "protonmail", "calendar", "contacts", "signal", "spotify", "zeal"
  , "chromium", "slack", "thunderbird", "freetube", "windy"
  ]

-- Search engines
ddg = searchEngine "DuckDuckGo" "https://duckduckgo.com/?q="
-- Search engine map
searchEngineMap method = M.fromList
    [ ((0, xK_g), method S.google)
    , ((0, xK_w), method S.wikipedia)
    , ((0, xK_m), method S.maps)
    , ((0, xK_d), method ddg)
    ]

-- TODO: Eek, this type. Can I do anything about this? See the TODO about using
-- XMonad.Prompt.Layout and CycleSelectedLayouts, which might be a nice way to sidestep the
-- problem. Though it could be better to just learn what I'm supposed to..
myLayoutModifier :: LayoutClass l Window => l Window
                    -> D.ModifiedLayout
                       AvoidStruts
                       (D.ModifiedLayout
                          WithBorder
                          (D.ModifiedLayout
                             SmartBorder
                             (MT.MultiToggle
                                (MT.HCons NOFRILLSDECO MT.EOT)
                                (MT.MultiToggle
                                  (MT.HCons StdTransformers MT.EOT) (D.ModifiedLayout Spacing l)))))
                       Window
myLayoutModifier = avoidStruts . noBorders . smartBorders . titleToggle . mirrorToggle . spacing
  where
    titleToggle = MT.mkToggle (MT.single NOFRILLSDECO) :: LayoutClass l a => l a -> MT.MultiToggle (MT.HCons NOFRILLSDECO MT.EOT) l a
    mirrorToggle = MT.mkToggle (MT.single MIRROR)
    --   -- Equal gaps between windows
    --   -- https://wiki.archlinux.org/title/Xmonad#Equally_sized_gaps_between_windows
    --   -- Removed the top screen border because polybar has its own padding.
    --   -- TODO: remove padding from polybar and let the window manager handle it because that
    --            will mean when we transform our layout, we won't get weird spacing (like we do at
    --            the time of writing). Either that or leave the polybar padding and just accept a
    --            slightly larger gap at the top.
    spacing = spacingRaw False (Border 0 0 space 0) True (Border 0 space 0 space) True

-- TODO: hotkeys for shrink/expand/IncMasterN; a lot of layouts respond to shrink/expand
namedLayouts :: SM.Map String (Layout Window)
-- TODO: should be able to map over the list with
--   SM.fromList $ map (Data.Bifunctor.second (Layout . myLayoutModifier))
-- but the types are tricky..
namedLayouts = SM.fromList
  [ ("cross"                , Layout . myLayoutModifier $ simpleCross)
  , ("dishes"               , Layout . myLayoutModifier $ Dishes 2 (1/6))
  , ("grid"                 , Layout . myLayoutModifier $ Grid)
  , ("full"                 , Layout . myLayoutModifier $ StateFull)
  , ("binaryspacepartition" , Layout . myLayoutModifier $ emptyBSP)
  , ("accordion"            , Layout . myLayoutModifier $ Accordion)
  , ("binarycolumn"         , Layout . myLayoutModifier $ BinaryColumn 1.0 50)
  , ("multicolumns"         , Layout . myLayoutModifier $ multiCol [3] 3 0.01 0.5)
  , ("columns"              , Layout . myLayoutModifier $ multiCol [1] 1 0.01 (-0.5))
  , ("multidishes"          , Layout . myLayoutModifier $ MultiDishes 1 3 (1/5))
  , ("onebig"               , Layout . myLayoutModifier $ OneBig (3/4) (3/4))
  , ("resizablethreecol"    , Layout . myLayoutModifier $ ResizableThreeCol 1 (3/100) (1/2) [])
  , ("resizablethreecolmid" , Layout . myLayoutModifier $ ResizableThreeColMid 1 (3/100) (1/2) [])
  , ("resizabletall"        , Layout . myLayoutModifier $ ResizableTall 1 (3/100) (1/2) [])
  , ("roledex"              , Layout . myLayoutModifier $ Roledex)
  , ("spiral"               , Layout . myLayoutModifier $ spiral (16/9))
  , ("tabbed"               , Layout . myLayoutModifier $ tabbedAlways D.shrinkText tabTheme)
  , ("masterandtabbed"      , Layout . myLayoutModifier $ tmsCombineTwoDefault Full $ tabbedAlways D.shrinkText tabTheme)
  , ("tabbedleft"           , Layout . myLayoutModifier $ tabbedLeftAlways D.shrinkText tabTheme)
  ]

------------------------------------------------------------------------
-- Themes and config

-- Border colors for unfocused and focused windows, respectively.
--
myNormalBorderColor  = "#002b36" -- Solarized dark background colour
myFocusedBorderColor = "#657b83" -- Solarized dark foreground colour

space = 5 :: Integer
activeColor = "#79d2a6"
inactiveColor = "#194d33"
urgentColor = "#fccb00"

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

myPromptKeys :: [((KeyMask, KeySym), P.XP ())]
myPromptKeys =
  [ ((controlMask, xK_c), P.quit)
  ]

promptTheme :: P.XPConfig
promptTheme = def
  { P.height            = 50
  , P.searchPredicate   = fuzzyMatch
  , P.sorter            = fuzzySort
  , P.promptKeymap      = foldr (uncurry M.insert) P.defaultXPKeymap myPromptKeys
  , P.bgColor           = inactiveColor
  , P.fgColor           = activeColor
  , P.bgHLight          = activeColor
  , P.fgHLight          = inactiveColor
  , P.promptBorderWidth = 0
  , P.alwaysHighlight   = True }

tabTheme = titleBarTheme
  { D.decoWidth = 500
  , D.urgentColor = urgentColor
  , D.urgentTextColor = inactiveColor
  }

emConf :: EasyMotionConfig
emConf = def {
               sKeys = PerScreenKeys $ SM.fromList [(0, [xK_a, xK_s, xK_d, xK_f]), (1, [xK_h, xK_j, xK_k, xK_l])]
             , maxChordLen = 1
             }

------------------------------------------------------------------------
-- Window decoration
-- Thanks to https://gist.github.com/Avaq/9691fa3f8b6538fb949570884e5ee91e#file-xmonad-hs-L121-L122
-- I've modified this from the original to incorporate window corner radii and window spacing (in a
-- rather hacky manner) on only the bottom decoration. For the other side decorations to function
-- correctly, it'll be necessary to modify them to support this also.
-- TODO: incorporate spacing and corner radii more sensibly.
newtype SideDecoration a = SideDecoration Direction2D deriving (Show, Read)

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

bottomBarDecorate :: Eq a => l a -> D.ModifiedLayout (D.Decoration SideDecoration D.DefaultShrinker) l a
bottomBarDecorate = D.decoration D.shrinkText bottomBarTheme (SideDecoration D)

data NOFRILLSDECO = NOFRILLSDECO deriving (Read, Show, Eq)
instance MT.Transformer NOFRILLSDECO Window where
  transform _ x k = k (noFrillsDeco D.shrinkText titleBarTheme x) (\(D.ModifiedLayout _ x') -> x')

------------------------------------------------------------------------
-- Some terminal helpers
-- TODO: we should shell-escape all the commands here
-- - https://hackage.haskell.org/package/shell-escape
-- - https://hackage.haskell.org/package/xmonad-contrib-0.17.0/docs/XMonad-Prompt-Shell.html
-- - https://hackage.haskell.org/package/xmonad-contrib-0.17.0/docs/XMonad-Util-Run.html
spawnInTerminal c = spawn $ "wezterm start " ++ c
spawnAlacrittyApp c = spawn $ "alacritty -t " ++ c ++ " -e " ++ c
spawnAlacrittyAppAndHold c = spawn $ "alacritty -t '" ++ c ++ "' -e zsh -ic \"" ++ c ++ "; read\""

------------------------------------------------------------------------
-- Key bindings. Add, modify or remove key bindings here.
-- Discussion of key codes in xmonad:
-- https://old.reddit.com/r/xmonad/comments/638pbj/how_to_find_out_any_keysym_instantly/
-- In nixos, finding a key:
--   nix-shell -p xorg.xev --run "xev -event keyboard"
--
myKeys conf@XConfig {XMonad.modMask = modm} = M.fromList $

    -- launch a terminal
    [ ((modm                , xK_Return), spawn $ XMonad.terminal conf)
    -- launch a terminal in an empty workspace
    , ((modm .|. shiftMask  , xK_Return), viewEmptyWorkspace >> spawn (XMonad.terminal conf))

    -- launch ephemeral vim
    , ((modm                , xK_v     ), spawnInTerminal "$EDITOR")
    -- launch ephemeral vim in an empty workspace
    , ((modm .|. shiftMask  , xK_v     ), viewEmptyWorkspace >> spawnAlacrittyApp "$EDITOR")

    -- launch ephemeral lazygit
    , ((modm                , xK_g     ), spawnAlacrittyApp "lazygit")
    -- launch ephemeral lazygit in an empty workspace
    , ((modm .|. shiftMask  , xK_g     ), viewEmptyWorkspace >> spawnAlacrittyApp "lazygit")

    -- launch rofi-pass
    , ((modm .|. shiftMask  , xK_p     ), spawn "rofi-pass")

    -- swap workspace with another selected by workspaceprompt
    , ((modm .|. shiftMask  , xK_s     ), workspacePrompt promptTheme (windows . swapWithCurrent))
    -- swap workspaces left/right
    , ((modm .|. shiftMask,   xK_h     ), swapTo Prev)
    , ((modm .|. shiftMask,   xK_l     ), swapTo Next)

    -- Jump to a layout not in the default list
    -- TODO: it would be better to
    -- - put all the layouts into the default layout list
    -- - replace the xK_space keybinding to `sendMessage NextLayout` with CycleSelectedLayouts on a couple of main layouts
    -- - use XMonad.Prompt.Layout to select layouts
    -- The problem was, this wasn't really working correctly when I tried it- though I'm unsure why
    , ((modm                , xK_x     ), P.mkXPrompt (Wor "") promptTheme (P.mkComplFunFromList' promptTheme (SM.keys namedLayouts)) (\s -> whenJust (SM.lookup s namedLayouts) setLayout))

    -- find an empty workspace
    , ((modm                , xK_period), viewEmptyWorkspace)

    -- open audio control
    , ((modm                , xK_a     ), spawnAlacrittyApp "ncpamixer")

    -- window tagging (m-a, 'a' for 'annotate')
    -- , ((modm,               xK_a     ), tagPrompt def (withFocused . addTag))
    -- , ((modm .|. shiftMask, xK_a     ), tagPrompt defaultXPConfig (`withTaggedGlobalP` gotoWindow))
    -- , ((modm .|. shiftMask, xK_a     ), tagPrompt defaultXPConfig (\s -> withTaggedGlobalP s shiftHere))
    -- , ((modm .|. shiftMask, xK_a     ), tagPrompt defaultXPConfig (\s -> shiftToScreen s))

    -- Window selection
    , ((modm                , xK_f     ), selectWindow emConf >>= flip whenJust (windows . W.focusWindow))

    -- TODO: this might be superior to the current xK_slash below
    -- , ((modm              , xK_slash ), xmonadPrompt promptTheme)
    -- move window to rofi-selected workspace
    -- TODO: should this correspond to the m-o binding below, and therefore me be (modm .|. shiftMask  , xK_o     )
    , ((modm .|. shiftMask  , xK_slash ), workspacePrompt promptTheme (windows . W.shift))
    -- TODO: seems we have to "take" `windows` twice? I.e. we have to
    --   \w -> windows (W.shift w) >> windows $ W.greedyView w
    -- instead of
    --   \w -> windows (W.shift w . W.greedyView w)
    -- this is a working implementation of what we want to achieve (see usage of this code below):
    --   doF . liftM2 (.) W.greedyView W.shift
    , ((modm .|. shiftMask .|. controlMask, xK_slash ), workspacePrompt promptTheme (\w -> windows (W.shift w) >> windows (W.greedyView w)))
    -- jump to rofi-selected workspace
    -- TODO: filter the non-named workspaces out of this list?
    , ((modm,                 xK_o     ), workspacePrompt promptTheme (windows . W.greedyView))

    -- lock screen with Win+L (lock buttons on keyboards send Win+L)
    , ((mod4Mask,             xK_l     ), spawn "loginctl lock-session $XDG_SESSION_ID")

    -- PrintScreen button to start flameshot
    -- TODO: we should perhaps take the screenshot then ask the user which action to take with it
    , ((noModMask,            xK_Print ), spawn "flameshot gui --clipboard --path /home/msk/screenshots/")
    , ((controlMask,          xK_Print ), spawn "ocr-screenshot")

    -- Draw on the visible screen
    , ((modm .|. shiftMask,   xK_d     ), spawn "gromit-mpx")

    -- View prev/next workspace
    , ((modm,                 xK_l     ), nextWS)
    , ((modm,                 xK_h     ), prevWS)

    -- cycle through recent workspaces in recently-used order
    -- documentation for this module is much better in 0.17.0.9 than it is in 0.17
    -- https://xmonad.github.io/xmonad-docs/xmonad-contrib-0.17.0.9/XMonad-Actions-CycleWorkspaceByScreen.html
    -- TODO: I only really use this to toggle between the two most recently used workspaces. See
    -- whether it's possible to only make this toggle.
    -- TODO: Better to use M-C-O and M-C-I? Perhaps not, because it's a little more difficult to
    -- toggle back and forward, and doesn't map to Windows well.
    , ((modm,                 xK_Tab   ), cycleWorkspaceOnCurrentScreen [xK_Alt_L] xK_Tab xK_p)

    -- launch application runner
    , ((modm,                 xK_p     ), spawn "rofi -show drun -display-drun \"> \"")

     -- Rotate through the available layout algorithms
    , ((modm,                 xK_space ), sendMessage NextLayout)

    --  Reset the layouts on the current workspace to default. Useful to update layouts after
    --  config update.
    , ((modm .|. shiftMask,   xK_space ), setLayout $ XMonad.layoutHook conf)

    -- Resize the layout ratios
    , ((modm .|. controlMask, xK_comma ), sendMessage Shrink)
    , ((modm .|. controlMask, xK_period), sendMessage Expand)

    -- Resize viewed windows to the correct size
    -- TODO: what does this do?
    , ((modm,                 xK_n     ), refresh)

    -- Jump straight to video call workspace
    , ((modm,                 xK_z     ), (windows . W.greedyView) "zoom")

    -- Turn volume up 10%
    , ((modm,                 xK_KP_Add ), spawn "pactl set-sink-volume $(pactl list short | grep RUNNING | cut -f1) +10%")
    -- XF86AudioRaiseVolume
    , ((noModMask,            0x1008ff13 ), spawn "pactl set-sink-volume $(pactl list short | grep RUNNING | cut -f1) +10%")

    -- Previous track
    -- XF86AudioPrev
    , ((noModMask,            0x1008ff16 ), spawn "dbus-send --print-reply --dest=org.mpris.MediaPlayer2.spotify /org/mpris/MediaPlayer2 org.mpris.MediaPlayer2.Player.Previous")

    -- Play/pause
    -- XF86AudioPlay
    , ((noModMask,            0x1008ff14 ), spawn "dbus-send --print-reply --dest=org.mpris.MediaPlayer2.spotify /org/mpris/MediaPlayer2 org.mpris.MediaPlayer2.Player.PlayPause")
    , ((modm,                 xK_KP_Begin ), spawn "dbus-send --print-reply --dest=org.mpris.MediaPlayer2.spotify /org/mpris/MediaPlayer2 org.mpris.MediaPlayer2.Player.PlayPause")

    -- Next track
    -- XF86AudioNext
    , ((noModMask,            0x1008ff17 ), spawn "dbus-send --print-reply --dest=org.mpris.MediaPlayer2.spotify /org/mpris/MediaPlayer2 org.mpris.MediaPlayer2.Player.Next")

    -- Turn volume down 10%
    , ((modm,                 xK_KP_Subtract ), spawn "pactl set-sink-volume $(pactl list short | grep RUNNING | cut -f1) -10%")
    -- XF86AudioLowerVolume
    , ((noModMask,            0x1008ff11 ), spawn "pactl set-sink-volume $(pactl list short | grep RUNNING | cut -f1) -10%")

    -- Toggle mute
    , ((modm,                 xK_KP_Insert ), spawn "pactl set-sink-mute $(pactl list short | grep RUNNING | cut -f1) toggle")
    , ((noModMask,            0x1008ff12 ), spawn "pactl set-sink-mute $(pactl list short | grep RUNNING | cut -f1) toggle")

    -- Move focus to the next window
    , ((modm,                 xK_j     ), windows W.focusDown)

    -- Move focus to the previous window
    , ((modm,                 xK_k     ), windows W.focusUp  )

    -- Swap the focused window with the next window. Particularly useful for tabbed layouts where
    -- easymotion doesn't work.
    , ((modm .|. shiftMask,   xK_j     ), windows W.swapDown  )

    -- Swap the focused window with the previous window. Particularly useful for tabbed layouts
    -- where easymotion doesn't work.
    , ((modm .|. shiftMask,   xK_k     ), windows W.swapUp    )

    -- Swap windows
    , ((modm,                 xK_s     ), do
                                            win      <- selectWindow emConf
                                            stack    <- gets $ W.index . windowset
                                            let match = find ((win ==) . Just . fst) $ zip stack [0 ..]
                                            whenJust match $ (\i -> swapNth i >> focusNth i) . snd)

    -- Open systemd TUI
    , ((modm .|. controlMask, xK_s     ), spawnAlacrittyAppAndHold "sysz")

    -- Toggle window titles
    , ((modm .|. shiftMask,   xK_t     ), sendMessage $ MT.Toggle NOFRILLSDECO)

    -- Toggle mirror layout
    , ((modm .|. shiftMask,   xK_m     ), sendMessage $ MT.Toggle MIRROR)

    -- Push window back into tiling
    , ((modm,                 xK_t     ), withFocused $ \w -> windows (\s -> if M.member w (W.floating s)
                                                                  then W.sink w s
                                                                  -- RationalRect constructor args are: x y w h
                                                                  else W.float w (W.RationalRect (1/4) (1/4) (1/2) (1/2)) s))

    -- Toggle the status bar gap
    -- Use this binding with avoidStruts from Hooks.ManageDocks.
    -- See also the statusBar function from Hooks.DynamicLog.
    --
    -- , ((modm              , xK_b     ), sendMessage ToggleStruts)

    -- TODO: Window bringer (xK_y for "yank"- does the metaphor work? Or xK_x for "delete this one"?)
    -- , ((modm              , xK_y     ), windowPrompt def Goto wsWindows)
    -- needs some form of --no-custom; perhaps this can be achieved with windowBringerConfig +
    -- gotoMenuConfig: rofi -dmenu -no-custom -i
    -- , ((modm              , xK_o     ), spawn "rofi -theme-str 'window {width: 45%;}' -show window -display-window \"> \"")
    , ((modm                , xK_slash ), windowPrompt promptTheme Goto allWindows)
    , ((modm                , xK_y     ), windowPrompt promptTheme Bring allWindows)
    -- , ((modm              , xK_o     ), gotoMenuConfig def { menuCommand = "rofi"
    --                                                        , menuArgs = ["-dmenu", "-no-custom", "-theme-str", "{width: 45%;}", "-p", "> "]
    --                                                        })
    -- , ((modm              , xK_o     ), gotoMenuConfig windowBringerConfig)

    -- Quit xmonad
    -- Never want to do this- can use a different VT if necessary
    -- , ((modm .|. shiftMask, xK_q     ), io (exitWith ExitSuccess))

    -- Restart xmonad
    , ((modm                , xK_q     ), spawn "xmonad --restart")

    -- Launch default command
    -- TODO: should probably define a single map from workspaces to workspace keys and default
    -- commands, instead of have to create a new list entry for each new workspace service
    , ((modm                , xK_c     ), do
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
            -- TODO: run these commands when I switch to the workspace- note that they're
            -- idempotent
            , "systemctl --user start zoom"
            , "systemctl --user start whatsapp"
            , "systemctl --user start gmail"
            , "systemctl --user start protonmail"
            , "systemctl --user start calendar"
            , "systemctl --user start contacts"
            , "systemctl --user start signal"
            , "systemctl --user start spotify"
            , "systemctl --user start zeal"
            , "systemctl --user start chromium"
            , "systemctl --user start slack"
            , "systemctl --user start thunderbird"
            , "systemctl --user start freetube"
            , "systemctl --user start windy"
            ]
      currWsName <- withWindowSet (pure . W.currentTag)
      let currWsIndex = elemIndex currWsName (XMonad.workspaces conf)
      whenJust (currWsIndex >>= \x -> commands ^? element x) spawn)
      -- whenJust (currWsIndex >>= (commands ^?) . element) spawn)

    , ((modm .|. shiftMask, xK_c     ),
      withFocused $ \w -> do
        withDisplay $ \d -> do
          let commands = SM.fromList
                [ ("firefox",          "systemctl --user stop firefox"    )
                , ("zoom",             "systemctl --user stop zoom"       )
                , ("whatsapp",         "systemctl --user stop whatsapp"   )
                , ("gmail",            "systemctl --user stop gmail"      )
                , ("protonmail",       "systemctl --user stop protonmail" )
                , ("calendar",         "systemctl --user stop calendar"   )
                , ("contacts",         "systemctl --user stop contacts"   )
                , ("Signal",           "systemctl --user stop signal"     )
                , ("Spotify",          "systemctl --user stop spotify"    )
                , ("Zeal",             "systemctl --user stop zeal"       )
                , ("Chromium-browser", "systemctl --user stop chromium"   )
                , ("slack",            "systemctl --user stop slack"      )
                , ("thunderbird",      "systemctl --user stop thunderbird")
                , ("FreeTube",         "systemctl --user stop freetube"   )
                , ("windy",            "systemctl --user stop windy"      )
                ]
          prop <- io $ getTextProperty d w wM_CLASS >>= wcTextPropertyToTextList d
          maybe (selectWindow def { txtCol = "#ff0000" } >>= (`whenJust` killWindow)) spawn (prop ^? element 1 >>= \cls -> SM.lookup cls commands))

    , ((modm .|. shiftMask .|. controlMask, xK_c     ),
      withFocused $ \w -> do
        withDisplay $ \d -> do
          let commands = SM.fromList
                [ ("firefox",          "systemctl --user restart firefox"    )
                , ("zoom",             "systemctl --user restart zoom"       )
                , ("whatsapp",         "systemctl --user restart whatsapp"   )
                , ("gmail",            "systemctl --user restart gmail"      )
                , ("protonmail",       "systemctl --user restart protonmail" )
                , ("calendar",         "systemctl --user restart calendar"   )
                , ("contacts",         "systemctl --user restart contacts"   )
                , ("Signal",           "systemctl --user restart signal"     )
                , ("Spotify",          "systemctl --user restart spotify"    )
                , ("Zeal",             "systemctl --user restart zeal"       )
                , ("Chromium-browser", "systemctl --user restart chromium"   )
                , ("slack",            "systemctl --user restart slack"      )
                , ("thunderbird",      "systemctl --user restart thunderbird")
                , ("FreeTube",         "systemctl --user restart freetube"   )
                , ("windy",            "systemctl --user restart windy"      )
                ]
          prop <- io $ getTextProperty d w wM_CLASS >>= wcTextPropertyToTextList d
          maybe (return ()) spawn (prop ^? element 1 >>= \cls -> SM.lookup cls commands))
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
    -- I.e. we have to
    --   \w -> windows (W.shift w) >> windows $ W.greedyView w
    -- instead of
    --   \w -> windows (W.shift w . W.greedyView w)
    -- this is a working implementation of what we want to achieve (see usage of this code below):
    --   doF . liftM2 (.) W.greedyView W.shift
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

-- TODO: tell the status bar what our current workspace dir is
--   - perhaps just the leaf directory
--   - perhaps let the status bar decide what to display
--   - perhaps the git repo?
--     I.e. the project root, i.e. the project we're working on- this is probably the most useful.
--     Though perhaps showing the full workspace dir starting from the project root, but trimming
--     intermediate directories above a certain directory length, e.g. for deep, silly java paths
--   - implement a search for workspacedir. I.e. if I have a workspace that has ~/.dotfiles/ as
--     workspace dir, I should be able to search by the project directory
-- TODO: workspacedir seems to not work when we're using a layout that doesn't exist in our
-- predefined set. For example, using:
--   , ((modm                , xK_x     ), P.mkXPrompt (Wor "") promptTheme (P.mkComplFunFromList' promptTheme (SM.keys namedLayouts)) (\s -> whenJust (SM.lookup s namedLayouts) setLayout))
-- to select the *Columns* layout then mirroring it appears to mean the workspacedir will not be
-- set correctly when we receive the ChDir message. I *think*. This *might* depend on exactly when
-- the workspacedir is set- before the mirror? after the mirror? before the workspace selection?
-- after?
myLayout = workspaceDir "/home/msk" standardLayout
         & onWorkspace "firefox" ffLayout
         & myLayoutModifier
  where
    -- TODO: hotkeys for jumping to tabs 0-9. This is probably just a hotkey to jump to window 0-9
    -- on the current workspace.
    -- TODO: show icons on the tabs, in particular:
    --   - Neovim
    --   - Terminal (Alacritty, Wezterm)
    --   - consider left-aligning tab titles if possible, if adding icons
    -- TODO: display urgency on tab titles
    standardLayout = tiledLayout ||| StateFull ||| tabbedLeftAlways D.shrinkText tabTheme
    ffLayout       = StateFull
    -- TODO: make tiled automatically put its fourth and subsequent windows in tabs? See the
    -- TallMastersCombo layout earlier for one possible means of doing this (though probably not-
    -- chances are it'll be necessary to use a different (sublayout?) module, or
    -- LayoutCombinators).
    tiledLayout    = Tall nmaster delta ratio
                   & bottomBarDecorate
    -- The default number of windows in the master pane
    nmaster = 1
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
    [ className =? "MPlayer"                       --> doFloat
    , className =? "Gimp"                          --> doFloat
    , className =? "Vmplayer"                      --> doFloat
    , resource  =? "desktop_window"                --> doIgnore
    , resource  =? "kdesktop"                      --> doIgnore
    , className =? "Navigator"                     --> doShift "firefox"
    , className =? "firefox"                       --> doShift "firefox"
    , className =? "Spotify"                       --> doShift "spotify"
    , className =? "spotify"                       --> doShift "spotify"
    , className =? "Signal"                        --> doShift "signal"
    , className =? "whatsapp"                      --> doShift "whatsapp"
    , className =? "zoom"                          --> viewShift "zoom"
    , className =? "Slack"                         --> doShift "slack"
    , className =? "slack"                         --> doShift "slack"
    , className =? "protonmail"                    --> doShift "protonmail"
    , className =? "gmail"                         --> doShift "gmail"
    , className =? "calendar"                      --> doShift "calendar"
    , className =? "contacts"                      --> doShift "contacts"
    , className =? "Zeal"                          --> doShift "zeal"
    , className =? "chromium-browser"              --> doShift "chromium"
    , className =? "Chromium-browser"              --> doShift "chromium"
    , className =? "thunderbird"                   --> doShift "thunderbird"
    , className =? "FreeTube"                      --> doShift "freetube"
    , className =? "windy"                         --> doShift "windy"
    , className =? "markdownpreview"               --> IP.insertPosition IP.Below IP.Older

    , fmap ("join?action=" `isPrefixOf`) className <&&> fmap ("join?action=" `isPrefixOf`) title --> doFloat -- Zoom info windows
    , stringProperty "WM_WINDOW_ROLE" =? "GtkFileChooserDialog" --> ask >>= doF . W.sink -- GTK file chooser dialog, such as firefox file upload
    ]
      where viewShift = doF . liftM2 (.) W.greedyView W.shift

------------------------------------------------------------------------
-- Event handling

-- Defines a custom handler function for X Events. The function should
-- return (All True) if the default handler is to be run afterwards. To
-- combine event hooks use mappend or mconcat from Data.Monoid.
--
myEventHook = swallowEventHook (className =? "Alacritty") (className =? "mpv"
                                                      <||> className =? "SimpleScreenRecorder"
                                                      <||> className =? "feh")
           -- See a fairly nice example of serverModeEventHookCmd' here: https://gist.github.com/czaplicki/37ab38da4245deaea8c86ceae3ff2fa2
           -- Use CHANGE_WORKSPACE_DIR with:
           --   xmonadctl -a CHANGE_WORKSPACE_WORKING_DIR "/home/msk"
           -- Note that this *does not work*:
           --   xmonadctl -a CHANGE_WORKSPACE_WORKING_DIR "~"
           <> serverModeEventHookF "CHANGE_WORKSPACE_WORKING_DIR" (sendMessage . Chdir)

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
-- Window activate hooks

-- Control action to take when a window requests focus
-- Ignore _NET_WM_ACTIVATE for Firefox, Chromium, Signal
-- Trying to prevent a window that's appearing on the current workspace from taking focus? Add a
-- manageHook, like this example that inserts windows with a classname of markdownpreview below the
-- current position, and focuses older windows:
--   , className =? "markdownpreview"               --> insertPosition Below Older
-- As in the case of "markdownpreview", That window may *also* be sending _NET_WM_ACTIVATE, so it
-- may also need to be added here.
-- Ref:
-- - https://xmonad.github.io/xmonad-docs/xmonad-contrib/XMonad-Hooks-EwmhDesktops.html#g:5
-- - https://hackage.haskell.org/package/xmonad-contrib-0.13/docs/XMonad-Hooks-InsertPosition.html
activateHook :: ManageHook
activateHook = className /=? "firefox"
          <&&> className /=? "Signal"
          <&&> className /=? "Slack"
          <&&> className /=? "Chromium-browser"
          <&&> className /=? "zoom"
          <&&> className /=? "markdownpreview" --> doFocus

------------------------------------------------------------------------
-- Run xmonad with the settings you specify. No need to modify this.
--
main = xmonad $ setEwmhActivateHook activateHook $ ewmh $ ewmhFullscreen $ docks defaults

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

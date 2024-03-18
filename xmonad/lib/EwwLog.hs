-- From:
-- https://github.com/shouya/user-config/blob/9b721c98a2defb27940d10e1d6350881923d23a9/xdg/xmonad/lib/XMonad/Hooks/EwwLog.hs
-- Thanks to the author (in case you try to read this, I looked for your contact details and could
-- not find them).

{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}

module EwwLog
  ( ewwStatusBar
  , ewwLayoutLog
  , ewwTitleLog
  , ewwWorkspaceLog
  )
where

import GHC.Generics

import Data.Function
import Data.Ord
import Data.Maybe
import Data.List
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Lazy.Char8 as LBC8

import qualified Data.Aeson as JS
import Data.Aeson.Types

import XMonad
import qualified XMonad.StackSet as S
import XMonad.Hooks.StatusBar
import XMonad.Hooks.UrgencyHook
import XMonad.Util.NamedWindows (getName)
import XMonad.Hooks.StatusBar.PP (WS(..))


data WorkspaceDesc = WorkspaceDesc
  { index      :: Int
  , name       :: String
  , visible    :: Bool
  , current    :: Bool
  , hidden     :: Bool
  , urgent     :: Bool
  , visibleNoW :: Bool
  }
  deriving (Show, Generic)


instance ToJSON WorkspaceDesc where
    toEncoding = genericToEncoding defaultOptions

makePPWS :: X WS
makePPWS = do
  winset <- gets windowset
  urgents <- readUrgents

  pure $ WS { wsWindowSet = winset
            , wsUrgents = urgents
            , wsWS = S.workspace $ S.current winset
            , wsPP = def
            }

workspaceDescriptions :: X [WorkspaceDesc]
workspaceDescriptions = do
  ppws <- makePPWS
  s <- gets windowset

  allWorkspaceTags <- asks (workspaces . config)
  let workspaces = map S.workspace (S.current s : S.visible s) ++ S.hidden s
  let wsIndex ws = fromMaybe (length allWorkspaceTags) $
                   elemIndex (S.tag ws) allWorkspaceTags
  let workspaces' = sortBy (comparing wsIndex) workspaces

  let parseWs ws = toDesc (wsIndex ws) (ppws {wsWS = ws})

  pure $ map parseWs workspaces'

  where toDesc :: Int -> WS -> WorkspaceDesc
        toDesc index ppws =
          WorkspaceDesc { index      = index
                        , name       = S.tag (wsWS ppws)
                        , visible    = isVisible' ppws
                        , urgent     = isUrgent ppws
                        , current    = isCurrent' ppws
                        , hidden     = isHidden' ppws
                        , visibleNoW = isVisibleNoWindows' ppws
                        -- TODO: hiddenNoWindows is the default, i.e. the fall-through state- could be
                        -- nice to communicate that to the caller/user
                        }


ewwLayoutLog :: X ()
ewwLayoutLog = pure ()

ewwTitleLog :: X ()
ewwTitleLog = do
  winset <- gets windowset
  title <- maybe (pure "") (fmap show . getName) . S.peek $ winset
  xmonadPropLog' "_XMONAD_TITLE_LOG" title


ewwWorkspaceLog :: X ()
ewwWorkspaceLog = do
  wsDescs <- workspaceDescriptions
  xmonadPropLog' "_XMONAD_WORKSPACE_LOG" (LBC8.unpack $ JS.encode wsDescs)


ewwStatusBar :: StatusBarConfig
ewwStatusBar = StatusBarConfig log (start) (pure ())
  where log = ewwLayoutLog <> ewwTitleLog <> ewwWorkspaceLog
        start = xmonadPropLog "helloworld"

----------------------
--- stolen from XMonad.Hooks.StatusBar.PP


-- | Predicate for urgent workspaces.
isUrgent :: WS -> Bool
isUrgent WS{..} = any (\x -> (== Just (S.tag wsWS)) (S.findTag x wsWindowSet)) wsUrgents

-- | Predicate for the current workspace. Caution: assumes default
-- precedence is respected.
isCurrent' :: WS -> Bool
isCurrent' WS{..} = S.tag wsWS == S.currentTag wsWindowSet

-- | Predicate for the current workspace.
isCurrent :: WS -> Bool
isCurrent = (not <$> isUrgent) <&&> isCurrent'

-- | Predicate for visible workspaces. Caution: assumes default
-- precedence is respected.
isVisible' :: WS -> Bool
isVisible' = isVisibleNoWindows' <&&> isJust . S.stack . wsWS

-- | Predicate for visible workspaces.
isVisible :: WS -> Bool
isVisible = (not <$> isUrgent) <&&> (not <$> isCurrent') <&&> isVisible'

-- | Predicate for visible workspaces that have no windows. Caution:
-- assumes default precedence is respected.
isVisibleNoWindows' :: WS -> Bool
isVisibleNoWindows' WS{..} = S.tag wsWS `elem` visibles
  where visibles = map (S.tag . S.workspace) (S.visible wsWindowSet)

-- | Predicate for visible workspaces that have no windows.
isVisibleNoWindows :: WS -> Bool
isVisibleNoWindows =
    (not <$> isUrgent)
        <&&> (not <$> isCurrent')
        <&&> (not <$> isVisible')
        <&&> isVisibleNoWindows'

-- | Predicate for non-empty hidden workspaces. Caution: assumes default
-- precedence is respected.
isHidden' :: WS -> Bool
isHidden' = isJust . S.stack . wsWS

-- | Predicate for hidden workspaces.
isHidden :: WS -> Bool
isHidden =
    (not <$> isUrgent)
        <&&> (not <$> isCurrent')
        <&&> (not <$> isVisible')
        <&&> (not <$> isVisibleNoWindows')
        <&&> isHidden'

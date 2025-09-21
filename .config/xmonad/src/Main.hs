{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, LambdaCase #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
module Main (main) where
import Text.Printf (printf)
import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks (docks, avoidStruts)
import XMonad.Layout.Spacing (spacingRaw, Border(..))
import XMonad.Util.Dmenu (menuMapArgs)
import XMonad.Util.EZConfig (mkKeymap)
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified XMonad.StackSet as W
import XMonad.Hooks.EwmhDesktops (ewmhFullscreen, ewmh)
import XMonad.Layout.LimitWindows (limitWindows, decreaseLimit, increaseLimit)
import XMonad.Actions.CycleWS (moveTo, WSType (Not), emptyWS, prevScreen, nextScreen, shiftPrevScreen, shiftNextScreen, hiddenWS)
import XMonad.Util.Types (Direction1D(..))
import Data.Maybe (fromMaybe)
import XMonad.Layout.BoringWindows
import XMonad.Hooks.ManageHelpers (isDialog, ($?), isInProperty, doHideIgnore)
import XMonad.Util.WindowProperties (getProp32)
import qualified Data.List as List
import Data.Monoid (All(All), appEndo)

main :: IO ()
main = xmonad
     $ docks
     $ ewmhMinimizeUnmappedWindows
     $ setEwmhActivateHook' doActivate
     $ ewmhFullscreen
     $ ewmh
     $ def {
  modMask            = mod3Mask,
  borderWidth        = 2,
  normalBorderColor  = "#444444",
  focusedBorderColor = "#ffff70",
  workspaces         = show <$> [1..9] ++ [0],
  keys = \conf -> mkKeymap conf $ [
    ("M3-q",        kill),
    ("M3-M1-q",     kill >> decreaseLimit),
    ("M3-m",        focusMaster),
    ("M3-<Return>", windows shiftMaster'),
    ("M3-j",        focusDown),
    ("M3-k",        focusUp),
    ("M3-M1-j",     swapDown),
    ("M3-M1-k",     swapUp'),
    ("M3-p",        sendMessage NextLayout),
    ("M3-i",        sendMessage Shrink),
    ("M3-o",        sendMessage Expand),
    ("M3-z",        withFocused $ windows . W.sink),
    ("M3-w",        jumpTo),
    ("M3-\\",       pull),
    ("M3-M1-\\",    pullAndReplace),
    ("M3-u",        unstash),
    ("M3-M1-u",     unstashAndReplace),
    ("M3-s",        stash),
    ("M3-M1-s",     stash >> decreaseLimit),
    ("M3-,",        decreaseLimit),
    ("M3-.",        increaseLimit),
    ("M3-M1-,",     decreaseLimit >> sendMessage (IncMasterN (-1))),
    ("M3-M1-.",     increaseLimit >> sendMessage (IncMasterN 1)),
    ("M3-[",        prevScreen),
    ("M3-]",        nextScreen),
    ("M3-M1-[",     shiftPrevScreen >> prevScreen),
    ("M3-M1-]",     shiftNextScreen >> nextScreen),
    ("M3--",        moveTo Prev $ Not emptyWS),
    ("M3-=",        moveTo Next $ Not emptyWS),
    ("M3-M1--",     moveTo Prev hiddenWS),
    ("M3-M1-=",     moveTo Next hiddenWS)
  ] ++ [
    (printf k i, f i) |
    (k, f) <- [("M3-v %s",  windows . W.greedyView),
               ("M3-S-v %s", windows . W.shift)],
    i      <- workspaces conf
  ],
  layoutHook = avoidStruts
             $ let border = Border 10 10 10 10
               in spacingRaw False border True border True
             $ let nmaster = 1
                   ratio   = 1 / 2
                   delta   = 3 / 100
                   tiled   = Tall nmaster delta ratio
               in boringAuto (limitWindows 2 tiled) ||| Full,
  manageHook = composeAll [isDialog --> doFloat,
                           isChromeScreensharingPopup --> doHideIgnore],
  logHook = propLog
}

propLog :: X ()
propLog = xmonadPropLog =<< dynamicLogString def {
  ppCurrent = printf "<fc=#ffff70>[<fc=#ffffbb>%s</fc>]</fc>",
  ppVisible = printf "<<fc=#ff5ba8>%s</fc>>",
  ppHidden  = printf "[<fc=#ff5ba8>%s</fc>]",
  ppWsSep   = "",
  ppTitle   = const "",
  ppLayout  = \case
    "Spacing Tall" -> "[]="
    "Spacing Full" -> "[M]"
    s              -> s
}

shiftMaster' :: W.StackSet i l a s sd -> W.StackSet i l a s sd
shiftMaster' = W.modify' $ \c -> case c of
  W.Stack _ [] []       -> c
  W.Stack t [] (x : xs) -> W.Stack x [] (t : xs)
  W.Stack t ls rs       -> W.Stack t [] (reverse ls ++ rs)

jumpTo :: X ()
jumpTo =   pullableWindows
       >>= promptForWin "Jump"
       >>= windows . jumpToWin

jumpBack :: X ()
jumpBack = return () -- TODO

jumpToWin :: (Eq s, Eq a, Eq i) => a -> W.StackSet i l a s sd -> W.StackSet i l a s sd
jumpToWin w = W.shiftMaster . W.focusWindow w

stash :: X ()
stash = windows sendToBottom

unstash :: X ()
unstash =   stashedWindows
        >>= promptForWin "Unstash"
        >>= windows . pullWin

unstashAndReplace :: X ()
unstashAndReplace =   stashedWindows
                  >>= promptForWin "Unstash & Replace"
                  >>= windows . \w -> pullWin w . sendToBottom

stashedWindows :: X [Window]
stashedWindows = do
  XState {windowset = ss, mapped = ms} <- get
  return $ filter (`Set.notMember` ms) (W.index ss)

pull :: X ()
pull =   pullableWindows
     >>= promptForWin "Pull"
     >>= windows . pullWin

pullAndReplace :: X ()
pullAndReplace =   pullableWindows
               >>= promptForWin "Pull & Replace"
               >>= windows . \w -> pullWin w . sendToBottom

pullWin :: Eq a => a -> W.StackSet i l a s sd  -> W.StackSet i l a s sd
pullWin w = W.insertUp w . W.delete' w

pullableWindows :: X [Window]
pullableWindows = withWindowSet $ \ss -> do
  wsUnmapped <- stashedWindows
  let us = (W.workspace <$> W.visible ss) ++ W.hidden ss
      ws =  wsUnmapped
         ++ concatMap (W.integrate' . W.stack) us
         ++ Map.keys (W.floating ss)
  return ws

promptForWin :: String -> [Window] -> X Window
promptForWin p ws = do
  ts <- mapM (runQuery title) ws
  let ns = printf "%i. " <$> ([1..] :: [Int])
  let ts' = [n ++ t | (t, n) <- zip ts ns]
  w <- dmenuMapLines (Map.fromList (zip ts' ws)) >>= \case
    Just w  -> return w
    Nothing -> fail "no window selected"
  withWindowSet $ \ss -> if W.member w ss
    then return w
    else fail "selected window no longer exists"
  where
    dmenuMapLines = menuMapArgs "dmenu" ["-p", p, "-i", "-l", "50"]

sendToBottom :: Eq a => W.StackSet i l a s sd -> W.StackSet i l a s sd
sendToBottom ss = case W.peek ss of
  Just c  -> insertBottom c ss
  Nothing -> ss

insertBottom :: Eq a => a -> W.StackSet i l a s sd -> W.StackSet i l a s sd
insertBottom w = W.modify (Just $ W.Stack w [] [])
                          (\s -> Just $ s {W.down = W.down s ++ [w]})
               . W.delete' w

swapUp' :: X () -- BoringWindows' own swapUp is broken
swapUp' = do
  XState {mapped = ms} <- get
  windows $ W.modify' $ \c -> case c of
    W.Stack _ []        [] -> c
    W.Stack t (x : ls') rs -> W.Stack t ls' (x : rs)
    W.Stack t []        rs ->
      let (ls', rs') = breakIf (`Set.notMember` ms) rs
      in W.Stack t ls' rs'

ewmhMinimizeUnmappedWindows :: XConfig a -> XConfig a
ewmhMinimizeUnmappedWindows c =
  c {handleEventHook = eventHook <> handleEventHook c}
  where
    eventHook :: Event -> X All
    eventHook e = All True <$ case e of
      MapNotifyEvent {ev_window = w} -> setWindowMinimized False w
      UnmapEvent {ev_window = w} -> setWindowMinimized True w
      _ -> return ()

    setWindowMinimized :: Bool -> Window -> X ()
    setWindowMinimized isMinimized w = withDisplay $ \dpy -> do
      _NET_WM_STATE        <- getAtom "_NET_WM_STATE"
      _NET_WM_STATE_HIDDEN <- getAtom "_NET_WM_STATE_HIDDEN"
      st <- fromMaybe [] <$> getProp32 _NET_WM_STATE w
      let st' = (if isMinimized then (:) else List.delete)
                (fromIntegral _NET_WM_STATE_HIDDEN)
                st
      io $ changeProperty32 dpy w _NET_WM_STATE aTOM propModeReplace st'

doActivate :: ManageHook
doActivate = ask >>= \w -> doF $ \ss ->
  let Just u = W.findTag w ss
  in if W.currentTag ss == u
      then pullWin w ss
      else jumpToWin w ss

-- HACK: setEwmhActivateHook does nothing
setEwmhActivateHook' :: ManageHook -> XConfig a -> XConfig a
setEwmhActivateHook' h c =
  c {handleEventHook = eventHook <> handleEventHook c}
  where
    eventHook :: Event -> X All
    eventHook ClientMessageEvent {ev_window = w, ev_message_type = t} = do
      _NET_ACTIVE_WINDOW <- getAtom "_NET_ACTIVE_WINDOW"
      if t == _NET_ACTIVE_WINDOW then do
        runQuery h w >>= windows . appEndo
        return (All False)
      else
        return (All True)
    eventHook _ = return (All True)

-- HACK: these things don't even have a WM_CLASS
--       so this is as good a guess as we can make
isChromeScreensharingPopup :: Query Bool
isChromeScreensharingPopup =
  title $? " is sharing your screen." <||>
  title $? " is sharing a window." <&&>
  isInProperty "_NET_WM_STATE" "_NET_WM_STATE_ABOVE"

breakIf :: (a -> Bool) -> [a] -> ([a], [a])
breakIf p xs = let (ls, rs) = loop [] xs
               in (reverse ls, rs)
  where loop ls []           = (ls, [])
        loop ls rs@(x : rs') =
          if p x then (ls, rs) else loop (x : ls) rs'

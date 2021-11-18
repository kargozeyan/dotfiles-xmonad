-- Main
import XMonad
import System.IO
import System.Exit
import qualified XMonad.StackSet as W

-- Actions
import XMonad.Actions.GridSelect
import XMonad.Actions.CycleWS (Direction1D(..), moveTo, shiftTo, WSType(..), nextScreen, prevScreen)
import XMonad.Actions.SpawnOn
import XMonad.Actions.MouseResize
import XMonad.Actions.WithAll (sinkAll, killAll)
import XMonad.Actions.CopyWindow (kill1)
import XMonad.Actions.Promote
import XMonad.Actions.RotSlaves (rotSlavesDown, rotAllDown)

-- Data
import Data.Semigroup
import Data.Monoid
import Data.Char
import Data.Maybe (fromJust)
import Data.Maybe (isJust)
import qualified Data.Map as M

-- Hooks
import XMonad.Hooks.DynamicProperty
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks (avoidStruts, docksEventHook, manageDocks, ToggleStruts(..))
import XMonad.Hooks.SetWMName
import XMonad.Hooks.ManageHelpers (isFullscreen, doFullFloat, isDialog, doCenterFloat, doRectFloat)

-- Layouts
import XMonad.Layout.Accordion
import XMonad.Layout.GridVariants (Grid(Grid))
import XMonad.Layout.SimplestFloat
import XMonad.Layout.Spiral
import XMonad.Layout.ResizableTile
import XMonad.Layout.Tabbed
import XMonad.Layout.ThreeColumns

--Layouts modifers
import XMonad.Layout.LayoutModifier
import XMonad.Layout.LimitWindows (limitWindows, increaseLimit, decreaseLimit)
import XMonad.Layout.Magnifier
import XMonad.Layout.MultiToggle (mkToggle, single, EOT(EOT), (??))
import XMonad.Layout.MultiToggle.Instances (StdTransformers(NBFULL, MIRROR, NOBORDERS))
import XMonad.Layout.NoBorders
import XMonad.Layout.Renamed
import XMonad.Layout.Simplest
import XMonad.Layout.Spacing
import XMonad.Layout.SubLayouts
import XMonad.Layout.WindowNavigation
import XMonad.Layout.WindowArranger (windowArrange, WindowArrangerMsg(..))
import qualified XMonad.Layout.ToggleLayouts as T (toggleLayouts, ToggleLayout(Toggle))
import qualified XMonad.Layout.MultiToggle as MT (Toggle(..))


-- Utilities
import XMonad.Util.Dmenu
import XMonad.Util.EZConfig(additionalKeysP)
import XMonad.Util.NamedScratchpad
import XMonad.Util.Scratchpad
import XMonad.Util.Run (runProcessWithInput, safeSpawn, spawnPipe)
import XMonad.Util.SpawnOnce
import Graphics.X11.ExtraTypes.XF86


------------------------------------------------------------------------
-- My defaults
------------------------------------------------------------------------
myFont :: String
myFont = "xft:Cascadia Code:regular:size=9:antialias=true:hinting=true"

myTerminal :: String
myTerminal = "alacritty"        

myModMask :: KeyMask
myModMask = mod4Mask   -- mode mask to Super key         

myBrowser :: String
myBrowser = "firefox-developer-edition"         

myBorderWidth :: Dimension
myBorderWidth = 0               -- Window border | NOTE: currently its 0, so there would not be borders

myNormalColor :: String
myNormalColor = "#4c566a"       -- Border colors for windows

myFocusColor :: String
myFocusColor  = "#eceff4"       -- Border colors of focused windows

------------------------------------------------------------------------
-- Space between Tiling Windows
------------------------------------------------------------------------
mySpacing :: Integer -> l a -> XMonad.Layout.LayoutModifier.ModifiedLayout Spacing l a
mySpacing i = spacingRaw False (Border (24 + i) i i i) True (Border i i i i) True


------------------------------------------------------------------------
-- Tiling Windows
------------------------------------------------------------------------
spirals  = renamed [Replace "Spirals"]
           $ smartBorders
           $ windowNavigation
           $ addTabs shrinkText myTabTheme
           $ subLayout [] (smartBorders Simplest)
           $ mySpacing 8
           $ spiral (6/7)
grid     = renamed [Replace "Grid"]
           $ smartBorders
           $ windowNavigation
           $ addTabs shrinkText myTabTheme
           $ subLayout [] (smartBorders Simplest)
           $ limitWindows 12
           $ mySpacing 8
           $ mkToggle (single MIRROR)
           $ Grid (16/10)
tall     = renamed [Replace "Tall"]
           $ smartBorders
           $ windowNavigation
           $ addTabs shrinkText myTabTheme
           $ subLayout [] (smartBorders Simplest)
           $ limitWindows 8
           $ mySpacing 8
           $ ResizableTall 1 (3/100) (1/2) []
magnify  = renamed [Replace "Magnify"]
           $ smartBorders
           $ windowNavigation
           $ addTabs shrinkText myTabTheme
           $ subLayout [] (smartBorders Simplest)
           $ magnifier
           $ limitWindows 12
           $ mySpacing 8
           $ ResizableTall 1 (3/100) (1/2) []
monocle  = renamed [Replace "Monocle"]
           $ smartBorders
           $ windowNavigation
           $ addTabs shrinkText myTabTheme
           $ subLayout [] (smartBorders Simplest)
           $ limitWindows 20 Full
threeCol = renamed [Replace "Three Col"]
           $ smartBorders
           $ windowNavigation
           $ addTabs shrinkText myTabTheme
           $ subLayout [] (smartBorders Simplest)
           $ limitWindows 7
           $ ThreeCol 1 (3/100) (1/2)
threeRow = renamed [Replace "Three Row"]
           $ smartBorders
           $ windowNavigation
           $ addTabs shrinkText myTabTheme
           $ subLayout [] (smartBorders Simplest)
           $ limitWindows 7
           $ Mirror
           $ ThreeCol 1 (3/100) (1/2)
tabs     = renamed [Replace "Tabs"]
           $ tabbed shrinkText myTabTheme
tallAccordion  = renamed [Replace "Tall"]
           $ Accordion
wideAccordion  = renamed [Replace "Wide A"]
           $ Mirror Accordion

myTabTheme = def { fontName            = myFont
                 , activeColor         = "#46d9ff"
                 , inactiveColor       = "#313846"
                 , activeBorderColor   = "#46d9ff"
                 , inactiveBorderColor = "#282c34"
                 , activeTextColor     = "#282c34"
                 , inactiveTextColor   = "#d0d0d0"
                 }

------------------------------------------------------------------------
-- Layout Hook
------------------------------------------------------------------------
myLayoutHook = avoidStruts $ mouseResize $ windowArrange
               $ mkToggle (NBFULL ?? NOBORDERS ?? EOT) myDefaultLayout
             where
               myDefaultLayout =      withBorder myBorderWidth tall
                                  ||| noBorders monocle
                                  ||| grid
                                  ||| spirals
                                  ||| noBorders tabs
                                  ||| magnify
                                  ||| threeCol
                                  ||| threeRow
                                  ||| tallAccordion
                                  ||| wideAccordion

------------------------------------------------------------------------
-- Workspaces
------------------------------------------------------------------------
xmobarEscape :: String -> String
xmobarEscape = concatMap doubleLts
  where
    -- doubleLts 'a' = "<<"
    doubleLts x = [x]

myWorkspaces :: [String]
myWorkspaces = clickable . (map xmobarEscape)
    $ ["web", "dev", "doc", "term", "sys", "chat", "mus"]
  where
    clickable l = ["<action=xdotool key super+" ++ show (i) ++ ">" ++ ws ++ "</action>" | (i, ws) <- zip [1 .. 7] l]

windowCount :: X (Maybe String)
windowCount = gets $ Just . show . length . W.integrate' . W.stack . W.workspace . W.current . windowset

------------------------------------------------------------------------
-- Scratch Pads
------------------------------------------------------------------------
myScratchPads :: [NamedScratchpad]
myScratchPads =
  [
      NS "discord"              "discord"              (appName =? "discord")                   (customFloating $ W.RationalRect 0.15 0.15 0.7 0.7)
    , NS "spotify"              "spotify"              (appName =? "spotify")                   (customFloating $ W.RationalRect 0.15 0.15 0.7 0.7)
    , NS "nautilus"             "nautilus"             (className =? "Org.gnome.Nautilus")      (customFloating $ W.RationalRect 0.15 0.15 0.7 0.7)
    , NS "ncmpcpp"              launchMocp             (title =? "ncmpcpp")                     (customFloating $ W.RationalRect 0.15 0.15 0.7 0.7)
    , NS "whatsapp-for-linux"   "whatsapp-for-linux"   (appName =? "whatsapp-for-linux")        (customFloating $ W.RationalRect 0.15 0.15 0.7 0.7)
    , NS "terminal"             launchTerminal         (title =? "scratchpad")                  (customFloating $ W.RationalRect 0.15 0.15 0.7 0.7)
  ]
  where
    launchMocp     = myTerminal ++ " -t ncmpcpp -e ncmpcpp"
    launchTerminal = myTerminal ++ " -t scratchpad"

------------------------------------------------------------------------
-- Custom Keys
------------------------------------------------------------------------
myKeys :: [(String, X ())]
myKeys = 
  [-- Xmonad 
  ("M-C-r", spawn "xmonad --recompile && xmonad --restart"), -- recompile and restart
  ("M-C-q", io exitSuccess),                                 -- exit xmonad

  -- Launchers
  ("M-p", spawn "dmenu_run -p Run -fn 'Cascadia Code' -h 24 -nb '#2e3440' -nf '#eceff4' -sb '#bf616a'"),       -- Dmenu
  ("M-d", spawn "rofi -show drun"), -- Rofi

  -- Window Management
  ("M-q",           kill1),                 -- closing current windows
  ("M-S-q",         killAll),               -- closing all windwos in workspace
  ("M-m",           windows W.focusMaster), -- Move focus to the master window
  ("M-j",           windows W.focusDown),   -- Move focus to the next window
  ("M-k",           windows W.focusUp),     -- Move focus to the prev window
  ("M-S-m",         windows W.swapMaster),  -- Swap the focused window and the master window
  ("M-S-j",         windows W.swapDown),    -- Swap focused window with next window
  ("M-S-k",         windows W.swapUp),      -- Swap focused window with prev window
  ("M-<Backspace>", promote),               -- Moves focused window to master, others maintain order
  ("M-S-<Tab>",     rotSlavesDown),         -- Rotate all windows except master and keep focus in place
  ("M-<Tab>",       rotAllDown),       
  ("M-t",           withFocused $ windows . W.sink),  -- toggle tiling | NOTE: floating is done by MOD + click
  ("M-S-t",         sinkAll),                         -- toggle tiling for all windows in workspace
  
  -- Applications
  ("M-w",        spawn myBrowser),                                  -- opening browser
  ("M-<Return>", spawn myTerminal),                                 -- opening terminal
  ("M-s",        spawn "flameshot gui"),                            -- opening flameshot (screenshot)

  -- System
  ("<XF86AudioMute>",         spawn "amixer set Master toggle"),     -- toggling volume
  ("<XF86AudioRaiseVolume>",  spawn "amixer set Master 5%+ unmute"), -- increasing volume by 5%
  ("<XF86AudioLowerVolume>",  spawn "amixer set Master 5%- unmute"), -- decreasing volume by 5%
  ("<XF86MonBrightnessUp>",   spawn "brightnessctl s +5%"),          -- increasing brightness by 5%
  ("<XF86MonBrightnessDown>", spawn "brightnessctl s 5%-"),          -- decreasing brightness by 5%

  ("M-C-l", spawn "/home/karen/.xmonad/scripts/lock.sh"), -- locking screen
  ("M-b",   spawn "xkb-switch -n")                          -- switching keyboard languages
  ]

------------------------------------------------------------------------
-- Moving between WS
------------------------------------------------------------------------
      where nonNSP          = WSIs (return (\ws -> W.tag ws /= "NSP"))
            nonEmptyNonNSP  = WSIs (return (\ws -> isJust (W.stack ws) && W.tag ws /= "NSP"))

------------------------------------------------------------------------
-- Floats
------------------------------------------------------------------------
myManageHook :: XMonad.Query (Data.Monoid.Endo WindowSet)
myManageHook = composeAll
     [ className =? "confirm"         --> doFloat
     , className =? "file_progress"   --> doFloat
     , className =? "dialog"          --> doFloat
     , className =? "download"        --> doFloat
     , className =? "error"           --> doFloat
     , className =? "notification"    --> doFloat
     , className =? "toolbar"         --> doFloat
     , (className =? "firefox" <&&> resource =? "Dialog") --> doFloat  -- Float Firefox Dialog
     , (className =? "firefox-developer-edition" <&&> resource =? "Dialog") --> doFloat  -- Float Firefox Dialog
     , isFullscreen -->  doFullFloat
     ] <+> namedScratchpadManageHook myScratchPads


myHandleEventHook :: Event -> X All
myHandleEventHook = dynamicPropertyChange "WM_NAME" (title =? "Spotify" --> floating)
        where floating = doRectFloat (W.RationalRect 0.15 0.15 0.7 0.7)

------------------------------------------------------------------------
-- Startup Hooks
------------------------------------------------------------------------
myStartupHook = do
    spawnOnce "$HOME/.xmonad/scripts/autostart.sh"
    setWMName "LG3D"


------------------------------------------------------------------------
-- Main Do
------------------------------------------------------------------------


main :: IO ()
main = do
        xmproc <- spawnPipe "/usr/bin/xmobar ~/.config/xmobar/xmobarrc"
        xmonad $ ewmh def
                { manageHook = myManageHook <+> manageDocks
                , logHook = dynamicLogWithPP $ namedScratchpadFilterOutWorkspacePP $ xmobarPP
                        { ppOutput = hPutStrLn xmproc
                         , ppCurrent = xmobarColor "#88c0d0" "" . \s -> "[ "++ s ++" ]"
                         , ppVisible = xmobarColor "#4c566a" "" 
                         , ppHidden = xmobarColor "#b48ead" "". \s -> s
                         , ppHiddenNoWindows = xmobarColor "#4c566a" ""
                         , ppTitle = xmobarColor "#c7c7c7" "" . shorten 60
                         , ppSep = " | "
                         , ppExtras  = [windowCount]
                         , ppOrder  = \(ws:l:_:_)  -> [ws, "[" ++l++ "]"]
                        }
                , modMask            = mod4Mask
                , layoutHook         = myLayoutHook
                , workspaces         = myWorkspaces
                , normalBorderColor  = myNormalColor
                , focusedBorderColor = myFocusColor
                , terminal           = myTerminal
                , borderWidth        = myBorderWidth
                , startupHook        = myStartupHook
                , handleEventHook    = myHandleEventHook
                } `additionalKeysP` myKeys
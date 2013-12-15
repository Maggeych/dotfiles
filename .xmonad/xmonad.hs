-------------------------------------------------------------------------------
-- xmonad.hs for xmonad-darcs
-- Author: Øyvind 'Mr.Elendig' Heggstad <mrelendig AT har-ikkje DOT net>
-------------------------------------------------------------------------------
-- Compiler flags --
{-# LANGUAGE NoMonomorphismRestriction #-}

-- Imports --
-- stuff
import XMonad hiding ( (|||) )
import qualified XMonad.StackSet as W
import qualified Data.Map as M
import System.Exit
import XMonad.Util.Run ( spawnPipe )
import System.IO ( hPutStrLn )

-- actions
import XMonad.Actions.GridSelect

-- hooks
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.UrgencyHook
import XMonad.Hooks.InsertPosition
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.SetWMName -- used for a minecraft fix, see startupHook
import XMonad.Hooks.FadeInactive
import XMonad.Hooks.EwmhDesktops

-- layouts
import XMonad.Layout.ResizableTile
import XMonad.Layout.PerWorkspace
import XMonad.Layout.Spacing
import XMonad.Layout.LayoutCombinators
import XMonad.Layout.Grid
import XMonad.Layout.Accordion
import XMonad.Layout.Named
import XMonad.Layout.NoBorders
import XMonad.Layout.Fullscreen
import XMonad.Layout.ToggleLayouts

-- Media keys
import Graphics.X11.ExtraTypes.XF86

-------------------------------------------------------------------------------
-- Main --
main :: IO ()
main = xmonad =<< statusBar cmd pp kb conf
    where
      uhook = withUrgencyHookC NoUrgencyHook urgentConfig
      cmd = "bash -c \"tee >(xmobar -x0) | xmobar -x1\""
      pp = customPP
      kb = toggleStrutsKey
      conf = uhook $ ewmh myConfig

-------------------------------------------------------------------------------
-- Configs --
myConfig = defaultConfig { workspaces = workspaces'
                         , modMask = modMask'
                         , borderWidth = borderWidth'
                         , normalBorderColor = normalBorderColor'
                         , focusedBorderColor = focusedBorderColor'
                         , terminal = terminal'
                         , keys = keys'
                         , layoutHook = layoutHook'
                         , manageHook = manageHook'
                         }

logHook' = fadeInactiveLogHook fadeAmount
      where fadeAmount = 0xDDDDDDDD

-------------------------------------------------------------------------------
-- Window Management --
manageHook' = composeAll [ isFullscreen             --> doFullFloat
                         --, className =? "MPlayer"   --> doFloat
                         , className =? "Gimp"      --> unfloat
                         --, className =? "Vlc"       --> doFloat
			 , className =? "Firefox"     --> doShift "web"
			 , className =? "Thunderbird" --> doShift "mail"
			 , className =? "Thunar" --> doShift "file"
			 , insertPosition Above Newer
			 , transience'
                         ]
			where unfloat = ask >>= doF . W.sink


-------------------------------------------------------------------------------
-- Looks --
-- bar
customPP = defaultPP { ppCurrent = xmobarColor "#58C1F5" "" . wrap "<" ">"
                     , ppVisible = xmobarColor "#58C1F5" ""
                     , ppHidden = xmobarColor "#888888" ""
                     , ppHiddenNoWindows = xmobarColor "#686964" ""
                     , ppUrgent = xmobarColor "#25DB4F" "" . wrap "[" "]"
                     , ppLayout = xmobarColor "#58C1F5" ""
                     , ppTitle = xmobarColor "#ED3764" "" . shorten 80
                     , ppSep = xmobarColor "#666666" "" " | "
                     }

-- GridSelect
myGSConfig = defaultGSConfig { gs_cellwidth = 160 }

-- urgent notification
urgentConfig = UrgencyConfig { suppressWhen = Focused, remindWhen = Dont }

-- borders
borderWidth' = 3
-- normalBorderColor'  = "#2E2C28"
-- focusedBorderColor' = "#CFB776"
normalBorderColor'  = "#000000"
focusedBorderColor' = "#58C1F5"

-- workspaces
workspaces' = ["misc", "web", "dev", "4", "5", "6", "file", "mail", "music"]
                                                             
-- layouts
layoutHook' = (toggleLayouts (noBorders (fullscreenFull Full)) (named "grid" grid ||| named "tall" tall ||| named "wide" wide))
  where
    grid = space $ Mirror $ GridRatio(3/4)
    tall = space $ ResizableTall 1 (5/100) (1/2) []
    wide = space $ Mirror $ ResizableTall 1 (5/100) (1/2) []
    space = spacing 2

-------------------------------------------------------------------------------
-- Terminal --
terminal' = "urxvt"

-------------------------------------------------------------------------------
-- Keys/Button bindings --
-- modmask
modMask' = mod4Mask

-- keys
toggleStrutsKey :: XConfig Layout -> (KeyMask, KeySym)
toggleStrutsKey XConfig {XMonad.modMask = modMask} = (modMask, xK_b)

keys' :: XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
keys' conf@(XConfig {XMonad.modMask = modMask}) = M.fromList $
    -- launching and killing programs
    [ ((modMask,               xK_Return), spawn $ XMonad.terminal conf) 
    , ((modMask,               xK_p     ), spawn "dmenu_run -b -p 'Run'") 
    , ((modMask .|. shiftMask, xK_m     ), spawn "thunderbird")
    , ((modMask .|. shiftMask, xK_c     ), kill)
    , ((modMask, xK_o), spawn "firefox")
    , ((modMask .|. shiftMask, xK_f), spawn "thunar")
    , ((modMask, xK_f), sendMessage ToggleStruts >> sendMessage ToggleLayout)

    -- grid
    , ((modMask,               xK_g     ), goToSelected myGSConfig)

    -- layouts
    , ((modMask,               xK_space ), sendMessage NextLayout)
    , ((modMask .|. shiftMask, xK_space ), setLayout $ XMonad.layoutHook conf)
    , ((modMask,               xK_F1    ), sendMessage $ JumpToLayout "grid")
    , ((modMask,               xK_F2    ), sendMessage $ JumpToLayout "accordion")
    , ((modMask,               xK_F3    ), sendMessage $ JumpToLayout "tiled")
    , ((modMask,               xK_F4    ), sendMessage $ JumpToLayout "full")
    
    -- floating layer stuff
    , ((modMask,               xK_t     ), withFocused $ windows . W.sink)

    -- refresh
    , ((modMask,               xK_n     ), refresh)

    -- focus
    , ((modMask,               xK_Tab   ), windows W.focusDown)
    , ((modMask,               xK_j     ), windows W.focusDown)
    , ((modMask,               xK_k     ), windows W.focusUp)
    , ((modMask,               xK_m     ), windows W.focusMaster)

    -- swapping
    , ((modMask .|. shiftMask, xK_Return), windows W.swapMaster)
    , ((modMask .|. shiftMask, xK_k     ), windows W.swapDown  )
    , ((modMask .|. shiftMask, xK_j     ), windows W.swapUp    )

    -- increase or decrease number of windows in the master area
    , ((modMask              , xK_comma ), sendMessage (IncMasterN 1))
    , ((modMask              , xK_period), sendMessage (IncMasterN (-1)))

    -- resizing
    , ((modMask,               xK_h     ), sendMessage Shrink)
    , ((modMask,               xK_l     ), sendMessage Expand)
    , ((modMask .|. shiftMask, xK_h     ), sendMessage MirrorShrink)
    , ((modMask .|. shiftMask, xK_l     ), sendMessage MirrorExpand)

    -- quit, or restart
    -- , ((modMask .|. shiftMask, xK_q     ), io (exitWith ExitSuccess))
    , ((modMask              , xK_q     ), spawn "xmonad --recompile; xmonad --restart")
    ]
    ++
    -- mod-[1..9] %! Switch to workspace N
    -- mod-shift-[1..9] %! Move client to workspace N
    [((m .|. modMask, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
    ++
    -- mod-[w,e] %! switch to twinview screen 1/2
    -- mod-shift-[w,e] %! move window to screen 1/2
    [((m .|. modMask, key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip [xK_e, xK_w] [0..]
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]

-------------------------------------------------------------------------------


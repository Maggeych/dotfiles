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

-- actions
import XMonad.Actions.GridSelect

-- hooks
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.UrgencyHook
import XMonad.Hooks.InsertPosition
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.SetWMName -- used for a minecraft fix, see startupHook

-- layouts
import XMonad.Layout.NoBorders
import XMonad.Layout.ResizableTile
import XMonad.Layout.PerWorkspace
import XMonad.Layout.Named
import XMonad.Layout.Tabbed
import XMonad.Layout.Spacing
import XMonad.Layout.ThreeColumns
import XMonad.Layout.LayoutCombinators
import XMonad.Layout.Spiral

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
    conf = uhook myConfig

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
                         -- , startupHook= setWMName "LG3D"  -- minecraft wont resize according to the window without this
                         , manageHook = manageHook'
                         }

-------------------------------------------------------------------------------
-- Window Management --
manageHook' = composeAll [ isFullscreen             --> doFullFloat
                         --, className =? "MPlayer"   --> doFloat
                         , className =? "Gimp"      --> unfloat
                         --, className =? "Vlc"       --> doFloat
			 , className =? "Vimprobable2"     --> doShift "2"
			 , className =? "Firefox"     --> doShift "2"
			 , className =? "Clementine"     --> doShift "5"
			 , className =? "Sylpheed" --> doShift "4"
			 , className =? "Thunar" --> doShift "3"
			 , insertPosition Above Newer
			 , transience'
                         ]
			where unfloat = ask >>= doF . W.sink


-------------------------------------------------------------------------------
-- Looks --
-- bar
customPP = defaultPP { ppCurrent = xmobarColor "#25DB4F" "" . wrap "<" ">"
                     , ppVisible = xmobarColor "#4CAD64" ""
                     , ppHidden = xmobarColor "#888888" ""
                     , ppHiddenNoWindows = xmobarColor "#686964" ""
                     , ppUrgent = xmobarColor "#25DB4F" "" . wrap "[" "]" 
                     , ppLayout = xmobarColor "#4CAD64" ""
                     , ppTitle =  xmobarColor "#999999" "" . shorten 80
                     , ppSep = xmobarColor "#666666" "" " | "
                     }
-- GridSelect
myGSConfig = defaultGSConfig { gs_cellwidth = 160 }

-- urgent notification
urgentConfig = UrgencyConfig { suppressWhen = Focused, remindWhen = Dont }

-- borders
borderWidth' = 1
normalBorderColor'  = "#666666"
focusedBorderColor' = "#4CAD64"

-- tabs
tabTheme1 = defaultTheme { decoHeight = 12
                         , activeColor = "#EFEBE7"
                         , activeBorderColor = "#A35861"
                         , activeTextColor = "#A35861"
			 , inactiveColor = "#EFEBE7"
			 , inactiveTextColor = "#666666"
                         , inactiveBorderColor = "#B3B3B3"
                         }

-- workspaces
workspaces' = ["1", "2", "3", "4", "5"]

-- layouts
layoutHook' =
   tiled ||| wide
  where
    tiled  = named "tall" $ smartBorders $ ResizableTall nmaster delta (1/2) [] 
    wide = named "wide" $ smartBorders $ Mirror $ ResizableTall nmaster delta (1/2) [] 
    -- Default number of windows in master pane
    nmaster = 1
    -- Percent of the screen to increment when resizing
    delta = 2/100 -- Default proportion of the screen taken up by main pane
    ratio = toRational (2/(1 + sqrt 5 :: Double))

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
    , ((modMask,               xK_p     ), spawn "dmenu_run") 
    -- , ((modMask .|. shiftMask, xK_p     ), spawn "gmrun")
    , ((modMask .|. shiftMask, xK_m     ), spawn "sylpheed")
    , ((modMask .|. shiftMask, xK_c     ), kill)
    , ((modMask, xK_o), spawn "firefox")
    , ((modMask, xK_f), spawn "thunar")

    -- grid
    , ((modMask,               xK_g     ), goToSelected myGSConfig)

    -- layouts
    , ((modMask,               xK_space ), sendMessage NextLayout)
    , ((modMask .|. shiftMask, xK_space ), setLayout $ XMonad.layoutHook conf)
    , ((modMask,               xK_F1    ), sendMessage $ JumpToLayout "tiled")
    , ((modMask,               xK_F2    ), sendMessage $ JumpToLayout "tile3")
    , ((modMask,               xK_F3    ), sendMessage $ JumpToLayout "full")
    
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

    -- Keyboard backlight
    , ((0,               xF86XK_KbdBrightnessUp), spawn "kbdbacklight up")
    , ((0,               xF86XK_KbdBrightnessDown), spawn "kbdbacklight down")

    -- Monitor backlight
    , ((0,               xF86XK_MonBrightnessUp), spawn "showbrightness")
    , ((0,               xF86XK_MonBrightnessDown), spawn "showbrightness")

    -- Volume control
    , ((0,               xF86XK_AudioRaiseVolume), spawn "volumectl raise")
    , ((0,               xF86XK_AudioLowerVolume), spawn "volumectl lower")
    , ((0,               xF86XK_AudioMute), spawn "volumectl mute")

    -- resizing
    , ((modMask,               xK_h     ), sendMessage Shrink)
    , ((modMask,               xK_l     ), sendMessage Expand)
    , ((modMask .|. shiftMask, xK_h     ), sendMessage MirrorShrink)
    , ((modMask .|. shiftMask, xK_l     ), sendMessage MirrorExpand)

    -- quit, or restart
    , ((modMask .|. shiftMask, xK_q     ), io (exitWith ExitSuccess))
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


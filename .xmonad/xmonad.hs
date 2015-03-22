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
import XMonad.Hooks.EwmhDesktops -- used for a minecraft fix, see startupHook
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

-- Theme
import SolarizedTheme

main :: IO ()
main = do
  dzenRightBar <- spawnPipe $ "conky -c ~/.xmonad/.conky_dzen | dzen2 -dock -ta r -w 450 -h 16 -x 2620 -bg '" ++ color_bar_bg ++ "' -fn '" ++ font_bar ++ "' -y 0"
  xmonad =<< statusBar cmd pp kb conf
    where
      uhook = withUrgencyHookC NoUrgencyHook urgentConfig
      cmd = "dzen2 -dock -w 1420 -h 16 -x 1200 -y 0 -ta l -fn '" ++ font_bar ++ "' -bg '" ++ color_bar_bg ++ "'"
      pp = customPP
      kb = toggleStrutsKey
      conf = uhook $ ewmh myConfig

myConfig = defaultConfig { workspaces = workspaces'
                         , modMask = modMask'
                         , borderWidth = border_width
                         , normalBorderColor = color_border_normal
                         , focusedBorderColor = color_border_focus
                         , terminal = terminal'
                         , keys = keys'
                         , layoutHook = layoutHook'
                         , manageHook = manageHook'
                         }

manageHook' = composeAll [ isFullscreen             --> doFullFloat
                         --, className =? "MPlayer"   --> doFloat
                         , className =? "Gimp"      --> unfloat
                         --, className =? "Vlc"       --> doFloat
			 , className =? "Firefox"     --> doShift "web"
			 , className =? "Thunderbird" --> doShift "web"
			 , className =? "Thunar" --> doShift "file"
			 , className =? "Clementine" --> doShift "music"
			 , insertPosition Above Newer
			 , transience'
                         ]
			where unfloat = ask >>= doF . W.sink


customPP = defaultPP { ppCurrent = dzenColor color_bar_ws_active_fg color_bar_ws_active_bg . wrap " " " "
                     , ppVisible = dzenColor color_bar_ws_visible_fg color_bar_ws_visible_bg . wrap " " " "
                     , ppHidden = dzenColor color_bar_ws_hidden_fg color_bar_ws_hidden_bg . wrap " " " "
                     , ppHiddenNoWindows = dzenColor color_bar_ws_hiddennowindows_fg color_bar_ws_hiddennowindows_bg . wrap " " " "
                     , ppUrgent = dzenColor color_bar_ws_urgent_fg color_bar_ws_urgent_bg . wrap "<" ">"
                     , ppLayout = dzenColor color_bar_layout_fg color_bar_layout_bg . 
                        (\x -> case x of
                         "tiled"	->	" ^i(/home/maggeych/.xmonad/dzen2/layout_tall.xbm) "
                         "grid"				->	" ^i(/home/maggeych/.xmonad/dzen2/grid.xbm) "
                         "full"				->	" ^i(/home/maggeych/.xmonad/dzen2/layout_full.xbm) "
                        )
                     , ppTitle =  dzenColor color_bar_title "" . shorten 80 . pad
                     , ppSep = dzenColor color_bar_sep "" "|"
                     , ppWsSep = dzenColor color_bar_sep "" ""
                     }

myGSConfig = defaultGSConfig { gs_cellwidth = 160 }

urgentConfig = UrgencyConfig { suppressWhen = Focused, remindWhen = Dont }

--workspaces' = ["^i(/home/maggeych/.xmonad/dzen2/arch_10x10.xbm)", "^i(/home/maggeych/.xmonad/dzen2/www.xbm)", "^i(/home/maggeych/.xmonad/dzen2/games.xbm)", "^i(/home/maggeych/.xmonad/dzen2/diskette.xbm)", "^i(/home/maggeych/.xmonad/dzen2/mail.xbm)"]
workspaces' = ["misc", "web", "dev", "4", "5", "6", "file", "mail", "music"]
                                                             
layoutHook' = lessBorders OtherIndicated (toggleLayouts (noBorders (fullscreenFull Full)) (named "grid" grid ||| named "tiled" tiled)) where
    grid = space $ Mirror $ GridRatio (9/16)
    tiled  = space $ ResizableTall 1 (5/100) (1/2) []
    space = spacing border_gap

terminal' = "urxvt"

-------------------------------------------------------------------------------
-- Keys/Button bindings --
-- modmask
modMask' = mod4Mask

-- xF86XK_TouchpadToggle :: KeySym
-- xF86XK_TouchpadToggle = 0x1008ffa9

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
    , ((modMask, xK_o), spawn "dwb")
    , ((modMask .|. shiftMask, xK_f), spawn "env TERMCMD=\"urxvt\" EDITOR=\"vim\" urxvt -name ranger -e ranger")
    , ((modMask, xK_f), sendMessage ToggleStruts >> sendMessage ToggleLayout)

    -- grid
    , ((modMask,               xK_g     ), goToSelected myGSConfig)
    -- Lockscreen
    , ((modMask, xK_0), spawn "slock")

    -- layouts
    , ((modMask,               xK_space ), sendMessage NextLayout)
    
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
    , ((modMask .|. shiftMask, xK_j     ), windows W.swapDown  )
    , ((modMask .|. shiftMask, xK_k     ), windows W.swapUp    )

    -- increase or decrease number of windows in the master area
    , ((modMask              , xK_comma ), sendMessage (IncMasterN 1))
    , ((modMask              , xK_period), sendMessage (IncMasterN (-1)))

    -- Keyboard backlight                                       
    , ((0,               xF86XK_KbdBrightnessUp), spawn "asus-kbd-backlight up")
    , ((0,               xF86XK_KbdBrightnessDown), spawn "asus-kbd-backlight down")

    -- Monitor backlight
    , ((0,               xF86XK_MonBrightnessUp), spawn "xbacklight -inc 10")
    , ((0,               xF86XK_MonBrightnessDown), spawn "xbacklight -dec 10")

    -- Touchpad toggle
    , ((modMask,               xK_d), spawn "touchpadtoggle")

    -- Volume control
    , ((0,               xF86XK_AudioRaiseVolume), spawn "amixer sset Master 10%+ && notify-volume")
    , ((0,               xF86XK_AudioLowerVolume), spawn "amixer sset Master 10%- && notify-volume")
    , ((0,               xF86XK_AudioMute), spawn "amixer sset Master toggle && notify-volume")

    -- resizing
    , ((modMask,               xK_h     ), sendMessage Shrink)
    , ((modMask,               xK_l     ), sendMessage Expand)
    , ((modMask .|. shiftMask, xK_h     ), sendMessage MirrorShrink)
    , ((modMask .|. shiftMask, xK_l     ), sendMessage MirrorExpand)

    -- quit, or restart
    , ((modMask .|. shiftMask, xK_q     ), io (exitWith ExitSuccess))
    , ((modMask              , xK_q     ), spawn "killall dzen2; xmonad --recompile; xmonad --restart")
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

module SolarizedTheme where
import Graphics.X11.Xlib.Types

-------------------------------------------------------------------------------
-- THEME --
-------------------------------------------------------------------------------
-- bar
color_bar_bg = "#93a1a1"
color_bar_sep = "#657b83"
color_bar_title = "#002b36"

color_bar_layout_bg = "#cb4b16"
color_bar_layout_fg = "#002b36"

color_bar_ws_active_fg = "#eee8d5"
color_bar_ws_active_bg = "#073642"
color_bar_ws_visible_fg = "#657b83"
color_bar_ws_visible_bg = "#073642"
color_bar_ws_hidden_fg = "#002b36"
color_bar_ws_hidden_bg = "#657b83"
color_bar_ws_hiddennowindows_fg = "#657b83"
color_bar_ws_hiddennowindows_bg = ""
color_bar_ws_urgent_fg = "#002b36"
color_bar_ws_urgent_bg = "#cb4b16"

-- borders
border_gap = 8:: Int
border_width = 2 :: Dimension
color_border_normal = "#002b36"
color_border_focus = "#cb4b16"

-- fonts
font_bar = "xft:DejaVu Sans Mono:style=Book:antialias=false:size=8"
------------------------------------------------------------------------------


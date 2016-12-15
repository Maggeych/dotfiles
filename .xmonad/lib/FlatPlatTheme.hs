module FlatPlatTheme where
import Graphics.X11.Xlib.Types

-------------------------------------------------------------------------------
-- THEME --
-------------------------------------------------------------------------------
-- bar
color_bar_bg = "#b7b7b7"
color_bar_sep = "#c0c0c0"
color_bar_title = "#4a4f50"

color_bar_layout_bg = "#666666"
color_bar_layout_fg = "#bbbbbb"

color_bar_ws_active_fg = "#4a4f50"
color_bar_ws_active_bg = "#87c794"
color_bar_ws_visible_fg = "#cb4b16"
color_bar_ws_visible_bg = ""
color_bar_ws_hidden_fg = "#4a4f50"
color_bar_ws_hidden_bg = ""
color_bar_ws_hiddennowindows_fg = "#888888"
color_bar_ws_hiddennowindows_bg = ""
color_bar_ws_urgent_fg = "#002b36"
color_bar_ws_urgent_bg = "#cb4b16"

-- borders
border_gap = 5:: Int
border_width = 1 :: Dimension
color_border_normal = "#cccccc"
color_border_focus = "#aaaaaa"

-- fonts
-- font_bar = "-*-roboto-light-*-*-*-13-*-*-*-*-*-*-1"
font_bar = "-*-ubuntu-*-*-*-*-11-*-*-*-*-*-*-1"
------------------------------------------------------------------------------


module YellowTheme where
import Graphics.X11.Xlib.Types

-------------------------------------------------------------------------------
-- THEME --
-------------------------------------------------------------------------------
-- bar
color_bar_bg = "#2E2C28"
color_bar_sep = "#9F8A4B"
color_bar_title = "#EEEEEE"

color_bar_layout_bg = "#9F8A4B"
color_bar_layout_fg = "#000000"

color_bar_ws_active_fg = "#000000"
color_bar_ws_active_bg = "#9F8A4B"
color_bar_ws_visible_fg = "#A3A6AB"
color_bar_ws_visible_bg = ""
color_bar_ws_hidden_fg = "#A3A6AB"
color_bar_ws_hidden_bg = "#555555"
color_bar_ws_hiddennowindows_fg = "#A3A6AB"
color_bar_ws_hiddennowindows_bg = ""
color_bar_ws_urgent_fg = "#000000"
color_bar_ws_urgent_bg = "#C7756E"

-- borders
border_gap = 10 :: Int
border_width = 5 :: Dimension
color_border_normal = "#2E2C28"
color_border_focus = "#CFB776"

-- fonts
font_bar = "Ubuntu:regular:size=8"
------------------------------------------------------------------------------


xmodmap .Xmodmap
xset +fp /usr/share/fonts/local
xset fp rehash
xsetroot -cursor_name left_ptr
xrandr --output DP-1 --off --output HDMI-0 --off --output DVI-I-2 --mode 1920x1080 --pos 1200x0 --rotate normal --output DVI-I-1 --off --output DVI-I-0 --off --output DVI-I-3 --mode 1600x1200 --pos 0x0 --rotate right --output DP-0 --off
feh --bg-fill ~/.wallpaper/calmity-retro_00443255.jpg
[[ -f ~/.Xresources ]] && xrdb -merge ~/.Xresources
firefox &
thunderbird &
clementine & 
thunar &
# dropboxd &
# exec ck-launch-session dbus-launch xmonad
devmon &
exec dbus-launch xmonad

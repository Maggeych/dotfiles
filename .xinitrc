xmodmap .Xmodmap
xset +fp /usr/share/fonts/local
xset fp rehash
xsetroot -cursor_name left_ptr
xrandr --output DP-1 --off --output HDMI-0 --off --output DVI-I-2 --mode 1920x1200 --pos 1200x0 --rotate normal --output DVI-I-1 --off --output DVI-I-0 --off --output DVI-I-3 --mode 1600x1200 --pos 0x0 --rotate right --output DP-0 --off
feh --bg-fill ~/.wallpaper/2.jpg
[[ -f ~/.Xresources ]] && xrdb -merge ~/.Xresources
firefox &
thunderbird &
clementine & 
thunar &
dropboxd &
# exec ck-launch-session dbus-launch xmonad
devmon &
trayer --edge top --align right --widthtype pixel --width 50 --SetDockType true --SetPartialStrut true --expand false --transparent true --alpha 1 --tint 0x2E2C28 --expand true --heighttype pixel --height 16 &
compton -c -r 6 -l "-8" -t "-8" -f -C -D 5 &
exec dbus-launch xmonad

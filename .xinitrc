xmodmap .Xmodmap
xset +fp /usr/share/fonts/local
xset fp rehash
xsetroot -cursor_name left_ptr
xrandr --output DVI-I-2 --right-of DVI-I-3
xrandr --output DVI-I-2 --mode 1920x1080 
xrandr --output DVI-I-3 --mode 1600x1200
xrandr --output DVI-I-3 --rotate right
feh --bg-tile ~/.wallpapers/patterns/bg-pattern.png
[[ -f ~/.Xresources ]] && xrdb -merge ~/.Xresources
opera &
balsa &
clementine & 
thunar &
dropboxd &
# exec ck-launch-session dbus-launch xmonad
exec dbus-launch xmonad

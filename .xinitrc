xmodmap .Xmodmap
xset +fp /usr/share/fonts/local
xset fp rehash
xsetroot -cursor_name left_ptr
feh --bg-tile ~/.wallpapers/patterns/bg-pattern.png
[[ -f ~/.Xresources ]] && xrdb -merge ~/.Xresources
exec dbus-launch xmonad

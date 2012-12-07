xmodmap .Xmodmap
xset +fp /usr/share/fonts/local
xset fp rehash
xsetroot -cursor_name left_ptr
feh --bg-center ~/.wallpapers/snow-city-landscape-girl-streets-1080x1920.jpg
[[ -f ~/.Xresources ]] && xrdb -merge ~/.Xresources
dropboxd &
exec dbus-launch xmonad

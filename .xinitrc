xmodmap .Xmodmap
xset +fp /usr/share/fonts/local
xset fp rehash
xsetroot -cursor_name left_ptr
feh --bg-tile ~/.wallpaper/02.jpg
[[ -f ~/.Xresources ]] && xrdb -merge ~/.Xresources
eval $(/usr/bin/gnome-keyring-daemon --start --components=gpg,pkcs11,secrets,ssh)
trayer --edge top --align right --widthtype pixel --width 50 --SetDockType true --SetPartialStrut true --expand false --transparent true --alpha 1 --tint 0x2E2C28 --expand true --heighttype pixel --height 16 &
nm-applet &
compton -c -r 6 -l "-8" -t "-8" -f -C -D 5 &
devmon &
exec xmonad

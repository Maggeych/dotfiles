#!/bin/sh
#
export _JAVA_AWT_WM_NOREPARENTING=1
xmodmap .Xmodmap
xset +fp /usr/share/fonts/local
xset fp rehash
xsetroot -cursor_name left_ptr
#feh --bg-tile ~/.wallpaper/02.jpg
#feh --bg-tile ~/.wallpaper/deviant/little_paris_by_jonas_daehnert-d56bwq5.jpg
sh ~/.fehbg &
[[ -f ~/.Xresources ]] && xrdb -merge ~/.Xresources
eval $(/usr/bin/gnome-keyring-daemon --start --components=gpg,pkcs11,secrets,ssh)
trayer --edge top --align right --widthtype pixel --width 50 --SetDockType true --SetPartialStrut true --expand false --transparent true --alpha 1 --tint 0x93a1a1 --expand true --heighttype pixel --height 16 &
nm-applet &
#compton -r 8 -l "-8" -t "-8" -f -c -C -G -D 5 &
#compton --config ~/.compton.conf &
unclutter &
devmon &
exec xmonad

#!/bin/sh
#
export _JAVA_AWT_WM_NOREPARENTING=1
xmodmap .Xmodmap
urxvtd -q -f -o
xset +fp /usr/share/fonts/local
xset fp rehash
xsetroot -cursor_name left_ptr
eval $(dbus-launch --sh-syntax --exit-with-session)
sh ~/.fehbg &
[[ -f ~/.Xresources ]] && xrdb -merge ~/.Xresources &
eval $(/usr/bin/gnome-keyring-daemon --start --components=gpg,pkcs11,secrets,ssh)
trayer --edge top --align right --widthtype pixel --width 50 --SetDockType true --SetPartialStrut true --expand false --transparent true --alpha 1 --tint 0xf1efee --expand true --heighttype pixel --height 21 &
nm-applet &
/usr/lib/polkit-gnome/polkit-gnome-authentication-agent-1 &
#compton -r 8 -l "-8" -t "-8" -f -c -C -G -D 5 &
compton --config ~/.compton.conf &
devmon &
thunar &
chromium &
urxvtc &
exec xmonad

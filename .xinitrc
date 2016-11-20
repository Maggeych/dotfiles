#!/bin/sh
#
export _JAVA_AWT_WM_NOREPARENTING=1
xmodmap .Xmodmap
urxvtd -q -f -o
xset +fp /usr/share/fonts/local
xset fp rehash
xsetroot -cursor_name left_ptr
#eval $(dbus-launch --sh-syntax --exit-with-session)
xrandr --output DP-1 --off --output HDMI-0 --off --output DVI-I-2 --mode 1920x1200 --pos 1200x0 --rotate normal --output DVI-I-1 --off --output DVI-I-0 --off --output DVI-I-3 --mode 1600x1200 --pos 0x0 --rotate right --output DP-0 --off
# sh ~/.fehbg &
hsetroot -solid "#021c1e"
eval $(/usr/bin/gnome-keyring-daemon --start --components=gpg,pkcs11,secrets,ssh)
/usr/lib/polkit-gnome/polkit-gnome-authentication-agent-1 &
[[ -f ~/.Xresources ]] && xrdb -merge ~/.Xresources
compton --config ~/.compton.conf &
chromium &
thunderbird &
clementine & 
thunar &
nm-applet &
devmon &
urxvtc &
trayer --edge top --align right --widthtype pixel --width 50 --SetDockType true --SetPartialStrut true --expand false --transparent true --alpha 1 --tint 0x021c1e --expand true --heighttype pixel --height 10 &
exec dbus-launch xmonad
# exec startxfce4

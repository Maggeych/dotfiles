#!/bin/sh
#
export TERM='xterm-256color'
export TERMCMD='urxvt'
export EDITOR='vim'
export _JAVA_AWT_WM_NOREPARENTING=1
PATH=~/Code/bin:$PATH
xmodmap .Xmodmap
urxvtd -q -f -o
xset +fp /usr/share/fonts/local
xset fp rehash
xsetroot -cursor_name left_ptr
xrandr --output DP-1 --off --output HDMI-0 --off --output DVI-I-2 --mode 1920x1200 --pos 1600x0 --rotate normal --output DVI-I-1 --off --output DVI-I-0 --off --output DVI-I-3 --mode 1920x1080 --pos 0x0 --rotate normal --output DP-0 --off
# eval $(dbus-launch --sh-syntax --exit-with-session)
sh ~/.fehbg &
[[ -f ~/.Xresources ]] && xrdb -merge ~/.Xresources &
eval $(/usr/bin/gnome-keyring-daemon --start --components=gpg,pkcs11,secrets,ssh)
trayer --monitor 1 --edge top --align right --widthtype pixel --width 550 --SetDockType true --SetPartialStrut true --expand false --transparent true --alpha 1 --tint 0xb7b7b7 --expand true --heighttype pixel --height 21 &
nm-applet &
/usr/lib/polkit-gnome/polkit-gnome-authentication-agent-1 &
#compton -r 8 -l "-8" -t "-8" -f -c -C -G -D 5 &
#unclutter -reset -idle 1 &
compton --config ~/.compton.conf &
devmon &
thunar &
firefox &
exec xmonad

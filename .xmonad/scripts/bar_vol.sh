#!/bin/bash

source $(dirname $0)/config.sh

AMASTER=`amixer get Master | awk 'END{gsub(/\[|\]|%/,""); print $4}'`
ASTAT=`amixer get Master | awk 'END{gsub(/\[|\]|%/,""); print $6}'`
ICON=""
COLOR=""

if [[ $ASTAT = "on" ]]; then
    ICON="spkr_01.xbm"
    COLOR=$foreground
    PERCBAR=`echo "$AMASTER"\
        | gdbar -bg $bar_bg -fg $foreground -h 10 -w 25`
else
    ICON="spkr_02.xbm"
    COLOR=$warning
    PERCBAR=`echo 0 \
        | gdbar -bg $bar_bg -fg $foreground -h 10 -w 25`
fi

ICON="^i(/home/maggeych/.xmonad/dzen2/$ICON)"
#echo "^fg($foreground)$ICON ^fg($COLOR)$PERCBAR"
echo "$ICON $AMASTER"

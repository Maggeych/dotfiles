#!/bin/bash
source $(dirname $0)/config.sh

USED=`free -m | awk 'NR == 2 {gsub(/%/,""); print $3}'`
MAX=`free -m | awk 'NR == 2 {gsub(/%/,""); print $2}'`
PERC=`echo $USED*100/$MAX | bc`

ICON="mem.xbm"
if [[ $PERC -gt 75 ]]; then
    PERCBAR=`echo -e "$PERC"\
        | gdbar -bg $bar_bg -fg $warning -h 10 -w 25`
else
    PERCBAR=`echo -e "$PERC"\
        | gdbar -bg $bar_bg -fg $foreground -h 10 -w 25`
fi

ICON='^i(/home/maggeych/.xmonad/dzen2/'"$ICON)"
echo "$ICON ${USED}M"

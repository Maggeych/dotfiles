#!/bin/bash
source $(dirname $0)/config.sh

n=$(($1 + 4))
PERC=`mpstat -P ALL 0 | awk '{gsub(//,""); print $4}' | sed -n "$n"p`

# ICON="cpu.xbm"
# if [[ $PERC > 75 ]]; then
#     PERCBAR=`echo -e "$PERC"\
#         | gdbar -bg $bar_bg -fg $warning -h 1 -w 20`
# else
#     PERCBAR=`echo -e "$PERC"\
#         | gdbar -bg $bar_bg -fg $foreground -h 1 -w 20`
# fi
# 
# ICON='^i(/home/maggeych/.xmonad/dzen2/'"$ICON)"
echo "${PERC%.*}%"

#!/bin/sh

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

# Alias {{{1
# ==============================================================================
alias ls='ls -ha --color=auto --group-directories-first'
alias ll='ls -lah'

alias nightlight='redshift -l 47.998882:7.831192 -O 3900 -g 0.8 -b 0.4'
alias eveninglight='redshift -l 47.998882:7.831192 -O 4800 -g 0.8 -b 0.7'
alias daylight='redshift -l 47.998882:7.831192 -O 6500 -g 1.0 -b 1.0'

alias c='clear'

alias out='pkill xmonad'

alias hiwi='worklog hiwi'
alias windows='VBoxManage startvm Windows10-Hiwi'

alias gst='git status'
alias gad='git add'
alias gcm='git commit -m'
alias gd='git diff'

alias light='xbacklight -set'

# Functions {{{1
# ==============================================================================
tomp3() { ffmpeg -i "${1}" -q:a 0 "${1%.*}.mp3"; }

# Directories {{{1
# ==============================================================================
alias mp='cd /home/maggeych/Code/multicamerasetup && vim'
alias mpb='cd /home/maggeych/Code/multicamerasetup/build'
alias lab='cd /media/sdc1-usb-WD_Elements_10B8/Uni/16-WS/AufnahmenMarkus/Setup3/Example'

# Environment {{{1
# ==============================================================================
export TERM='xterm-256color'
export TERMCMD='urxvt'
export EDITOR='vim'
export JAVA_AWT_WM_NOREPARENTING=1

# Looks {{{1
# ==============================================================================
BASE16_SHELL="$HOME/.terminalcolors/base16-shell/base16-atelierforest.light.sh"
[[ -s $BASE16_SHELL ]] && source $BASE16_SHELL
PS1="\[\033[1;10m\]\u[\h]\[\033[0;10m\][\w]\[\033[1;96m\]\[\033[0m\]$ "
#PS1="\[\033[1;7m\] \u \[\033[0;7m\]\w \[\033[1;7m\]\[\033[0m\]"

if [ $(( RANDOM % 6)) -eq 0 ]; then
  echo -e -n "\033[0;37m"
  fortune -n 140
  echo -e "\033[0m"
fi

# ROS {{{1
# ==============================================================================
indigo() {
  source /opt/ros/indigo/setup.bash
  export ROS_PACKAGE_PATH=/home/maggeych/Code/ros:$ROS_PACKAGE_PATH
  export PYTHONPATH=/opt/ros/indigo/lib/python2.7/site-packages:$PYTHONPATH
  export PKG_CONFIG_PATH="/opt/ros/indigo/lib/pkgconfig:$PKG_CONFIG_PATH"
  export ROS_HOSTNAME="localhost"
  export ROS_MASTER_URI="http://localhost:11311"
  alias python=/usr/bin/python2
  alias catkin_make='catkin_make -DPYTHON_EXECUTABLE=/usr/bin/python2 -DPYTHON_INCLUDE_DIR=/usr/include/python2.7 -DPYTHON_LIBRARY=/usr/lib/libpython2.7.so'

  # # Gazebo
  # source /usr/share/gazebo/setup.sh
}
kuka () {
  indigo
  cd /home/maggeych/Uni/Hiwi/ros/
  vim
}


# Smarties {{{1
# ==============================================================================
cameraNetwork() {
  sudo ip link set enp0s20u2 up
  sudo ip addr add 192.168.1.66/255.255.255.0 broadcast 192.168.1.255 dev enp0s20u2
}
syncCamera() {
  rsync -a /home/maggeych/Code/smarties/root/VCpro-Z-0015/usr/local root@192.168.1.25:/usr/local
}

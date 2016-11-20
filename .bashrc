#!/bin/sh

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

# Alias {{{1
# ==============================================================================
alias ls='ls -ha --color=auto --group-directories-first --block-size=K'
alias ll='ls -lah'

alias nightlight='redshift -l 47.998882:7.831192 -O 3900 -g 0.8 -b 0.4'
alias eveninglight='redshift -l 47.998882:7.831192 -O 4800 -g 0.8 -b 0.7'
alias daylight='redshift -l 47.998882:7.831192 -O 6500 -g 1.0 -b 1.0'

alias c='clear'

alias gst='git status'
alias gad='git add'
alias gcm='git commit -m'
alias gd='git diff'

alias light='xbacklight -set'

# Functions {{{1
# ==============================================================================
tomp3() { ffmpeg -i "${1}" -q:a 0 "${1%.*}.mp3"; }
sgit() { git add "${1}" && git commit -m "${2}"; }

# Directories {{{1
# ==============================================================================
# alias sm='cd /home/maggeych/Code/smarties/smarties'
# alias smb='cd /home/maggeych/Code/smarties/build/x86-64/smarties'

# Environment {{{1
# ==============================================================================
export TERM='xterm-256color'
export TERMCMD='urxvt'
export EDITOR='vim'
export _JAVA_AWT_WM_NONREPARENTING=1
export JAVA_AWT_WM_NOREPARENTING=1

# Looks {{{1
# ==============================================================================
BASE16_SHELL="$HOME/.terminalcolors/base16-shell/base16-atelierforest.light.sh"
[[ -s $BASE16_SHELL ]] && source $BASE16_SHELL
# PS1="\[\033[1;10m\]/\h/\u\[\033[0;10m\] at \w\[\033[1;96m\]\n\[\033[0m\]$ "
PS1="\[\033[1;7m\] \u \[\033[0;7m\]\w \[\033[1;7m\]\[\033[0m\]"

if [ $(( RANDOM % 6)) -eq 0 ]; then
  echo -e -n "\033[0;37m"
  fortune -n 140
  echo -e "\033[0m"
fi

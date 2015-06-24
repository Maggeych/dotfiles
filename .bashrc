#!/bin/sh

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

# Alias {{{1
# ==============================================================================
alias ls='ls --color=auto --group-directories-first --block-size=K'
alias ll='ls -lah'

alias nightlight='redshift -l 47.998882:7.831192 -O 3900 -g 0.8 -b 0.4'
alias eveninglight='redshift -l 47.998882:7.831192 -O 4800 -g 0.8 -b 0.7'
alias daylight='redshift -l 47.998882:7.831192 -O 6500 -g 1.0 -b 1.0'

alias c='clear'

# Environment {{{1
# ==============================================================================
export TERM='xterm-256color'
export TERMCMD='urxvt'
export EDITOR='vim'
export _JAVA_AWT_WM_NOREPARENTING=1

# Looks {{{1
# ==============================================================================
PS1="\n\[\033[1;96m\]\u@\h\[\033[0m\]:\w\[\033[1;96m\]\n\[\033[1;0m\]$ \[\033[0m\]"

#if [ $(( RANDOM % 6)) -eq 0 ]; then
  echo -e "\033[0;32m"
  fortune -n 140
  echo -e "\033[0m"
#fi

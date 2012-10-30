
# ~/.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

alias ls='ls --color=auto'
alias ll='ls -la'
alias nightlight='redshift -l 47.998882:7.831192 -O 3900 -g 0.8 -b 0.4'
alias eveninglight='redshift -l 47.998882:7.831192 -O 4800 -g 0.8 -b 0.7'
alias daylight='redshift -l 47.998882:7.831192 -O 6500 -g 1.0 -b 1.0'
alias c='clear'
PS1='\[\033[1;34m\][\T] \[\033[00;31m\]\W\[\033[0;31m\] :\[\033[0m\] '
export TERM='xterm-256color'
export EDITO='vim'
#change on local master

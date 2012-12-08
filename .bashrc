# ~/.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

alias ls='ls --color=auto --group-directories-first --block-size=K'
alias ll='ls -la'
alias c='clear'
#PS1='\[\033[1;34m\][\T] \[\033[00;31m\]\W\[\033[0;31m\] :\[\033[0m\] '
#PS1='\[\033[1;30m\]\W \[\033[0;33m\]>\[\033[0m\] '
OUTLINE="\[\e[0;33m\]"
#PS1="\[\e[0;32m\]┌─ ‖\[\e[1;37m\]\u\[\e[0;32m\]|\h\[\e[0;32m\]‖ \[\e[0;37m\][\w]\n\[\e[0;32m\]└──── \[\e[0m\]"
PS1="${OUTLINE}┌─ ‖\[\e[0;37m\]\u${OUTLINE}|\[\e[1;37m\]\h${OUTLINE}‖ [\w]\n${OUTLINE}└──── \[\e[0m\]"
PS2="   └─── "
export TERM='xterm-256color'
export EDITOR='vim'

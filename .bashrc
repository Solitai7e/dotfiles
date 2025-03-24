[[ $- != *i* ]] && return

PS1='[\[\e[01;32m\]\u@\h \[\e[01;34m\]\W\[\e[00m\]] \[\e[01;33m\]\$\[\e[00m\] '
HISTCONTROL=ignoredups:erasedups

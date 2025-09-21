[[ $- != *i* ]] && return

PS0='\[\e]0;$(history 1 | sed "s/  [0-9]\{1,\}  //" | tr -s "\n" " ")\a\]'
PS1='[\[\e[01;32m\]\u@\h \[\e[01;34m\]\W\[\e[00m\]] \[\e[01;33m\]\$\[\e[00m\] '
HISTCONTROL=ignoredups:erasedups

set -o vi
. /usr/share/bash-completion/bash_completion 2> /dev/null || :

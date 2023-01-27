[[ $- != *i* ]] && return

PS1='[\[\e[01;32m\]\u@\h \[\e[01;34m\]\W\[\e[00m\]] \[\e[01;33m\]\$\[\e[00m\] '
HISTCONTROL=ignoredups:erasedups


ls()   { command ls -hal --color=auto "$@"; }
grep() { command grep --color=auto "$@"; }
diff() { command diff -u --color=auto "$@"; }

sanemod() {
    find "$@" \
        -type f -exec chmod -v 0644 {} + -o \
        -type d -exec chmod -v 0755 {} +
}

system-update() {
    sudo apt update
    sudo apt full-upgrade --autoremove --purge
    sudo apt clean
}

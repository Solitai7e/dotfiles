[ -f ~/.profile ] && . ~/.profile
[ -f ~/.bashrc  ] && . ~/.bashrc

if [ "$SHLVL" = 1 ] && [ "$(tty)" = "/dev/tty1" ]; then
  pgrep -U "$USER" -x Xorg > /dev/null || startx
fi

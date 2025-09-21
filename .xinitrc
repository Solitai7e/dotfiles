export NO_AT_BRIDGE=1
export GTK_A11Y=none

export QT_QPA_PLATFORMTHEME=gtk2

export XMODIFIERS=@im=fcitx
export GTK_IM_MODULE=fcitx
export QT_IM_MODULE=fcitx

exec dbus-launch --exit-with-x11 xsession

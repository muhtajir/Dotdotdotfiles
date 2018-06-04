fcitx -r
xset -b &
numlockx on &
(sleep 2 && xkbcomp ~/.config/xkb/custom.xkb "$DISPLAY") &

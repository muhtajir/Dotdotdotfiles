ibus-daemon -xrd &
xset -b &
numlockx on &
(sleep 4 && xkbcomp ~/.config/xkb/custom.xkb "$DISPLAY") &

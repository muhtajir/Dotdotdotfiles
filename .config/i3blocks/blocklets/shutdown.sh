echo ''
if [[ $BLOCK_BUTTON = 1 ]]; then
    yad --button=' _Herunterfahren':'systemctl poweroff' \
        --button=' _Neustarten':'systemctl reboot'\
        --button=' _Ruhezustand':'systemctl suspend'\
        --button=gtk-close \
        --title=Herunterfahren \
        --text='System herunterfahren?\n' \
        --text-align=center
fi

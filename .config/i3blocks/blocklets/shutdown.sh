echo ''
if [[ $BLOCK_BUTTON = 1 ]]; then
    yad --button=' _Herunterfahren':21 \
        --button=' _Neustarten':22 \
        --button=' _Bereitschaft':23 \
        --button=gtk-close \
        --title=Herunterfahren \
        --text='System herunterfahren?\n' \
        --text-align=center

    case $? in
        21)
            systemctl poweroff
            ;;
        22)
            systemctl reboot
            ;;
        23)
            systemctl suspend
            ;;
    esac
fi

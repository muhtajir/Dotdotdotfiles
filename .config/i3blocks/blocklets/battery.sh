source $(dirname $(realpath $0))/colors

battery=/sys/class/power_supply/${BLOCK_INSTANCE}
charge=$(grep '^POWER_SUPPLY_CAPACITY=' ${battery}/uevent | sed -r 's/^.+=//')
status=$(grep '^POWER_SUPPLY_STATUS=' ${battery}/uevent | sed -r 's/^.+=//')

status=$(cat ${battery}/status)
if [[ $status = 'Full' ]]; then
    echo ''
    exit
elif [[ $status = 'Charging' ]]; then
    echo -n '<span font_size="x-small"> </span>'
fi

if (( $charge < 5 )); then
    echo ""
    # change color if critical charge is reached
    echo ""
    echo \#$BASE08
elif (( $charge < 13 )); then
    echo ""
elif (( $charge < 36 )); then
    echo ""
elif (( $charge < 65 )); then
    echo ""
elif (( $charge < 92 )); then
    echo ""
else
    echo ""
fi


# notification warning system
if [[ $status != 'Charging' ]]; then
    if (( $charge >= 10 )); then
        [[ -e /tmp/batwarning_low.i3blocks ]] && rm /tmp/batwarning_low.i3blocks
        [[ -e /tmp/batwarning_critical.i3blocks ]] && rm /tmp/batwarning_critical.i3blocks
    elif [[ $charge -le 10 && ! -e /tmp/batwarning_low.i3blocks ]]; then
        notify-send -u normal 'Battery' "$charge percent remaining!"
        [[ -e /tmp/batwarning_critical.i3blocks ]] && rm /tmp/batwarning_critical.i3blocks
    elif [[ $charge -le 5 && ! -e /tmp/batwarning_critical.i3blocks ]]; then
        notify-send -u critical 'Battery' "Only $charge percent remaining!"
    fi
fi

# also send notification on click
if [[ -n "$BLOCK_BUTTON" ]]; then
    notify-send -u normal 'Batterie' "$charge percent remaining."
fi

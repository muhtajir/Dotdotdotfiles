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
    echo "#dd464c"
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

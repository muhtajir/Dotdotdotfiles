#!/bin/bash

case $BLOCK_BUTTON in
    4)
        light -A 5
        ;;
    5)
        light -U 5
        ;;
esac

l_value=$(light | sed -r 's/\..+//')

if (( $l_value > 99 )); then
    echo ""
elif (( $l_value > 0 )); then
    echo "<span size='xx-small'>${l_value}</span>"
else
    echo ""
fi

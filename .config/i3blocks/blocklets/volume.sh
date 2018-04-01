#!/bin/bash

# beware: unlike amixer, pactl allows raising the volume above 100%
case $BLOCK_BUTTON in
    4)
        pactl set-sink-volume ${BLOCK_INSTANCE} +5%
        ;;
    5)
        pactl set-sink-volume ${BLOCK_INSTANCE} -5%
        ;;
esac

volume=$(amixer -c ${BLOCK_INSTANCE} -M -D pulse get Master | grep -Eo '[[:digit:]]+%' -m 1)

# first check for mute state
if [[ $(amixer -c ${BLOCK_INSTANCE} -M -D pulse get Master | grep -E '^\s*Front.+\[off\]') ]]; then
    echo "<span size='x-small'>⛔</span>"
    exit
fi

if (( ${volume%%%} > 100 )); then
    echo "<span foreground=\"#${__BASE08}\"></span><span size='x-small'>${volume%%%}</span>"
elif (( ${volume%%%} > 99 )); then
    echo ""
elif (( ${volume%%%} > 0 )); then
    echo "<span size='x-small'>${volume%%%}</span>"
else
    echo ""
fi

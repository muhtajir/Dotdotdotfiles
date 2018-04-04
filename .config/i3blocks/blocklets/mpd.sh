#!/bin/bash

if systemctl --user is-active mpd.service > /dev/null; then
    if [ $BLOCK_BUTTON = 1 ]; then
        mpc toggle >&2
    fi

    song=$(mpc -f '%artist% - %title%'| head -n 1)
    random=$(mpc | grep -Eo 'random:[[:space:]]+on')
    echo "$song${random:+ <span size=\"small\" foreground=\"#${__BASE0E}\">R</span>}"
fi

#!/bin/bash

if systemctl --user is-active mpd.service > /dev/null; then
    if [ "$BLOCK_BUTTON" = 1 ]; then
        mpc toggle >&2
    fi

    song=$(mpc -f '%artist% - %title%' current | sed 's/&/\&amp;/g; s/</\&lt;/g; s/>/\&gt;/g; s/"/\&quot;/g; s/'"'"'/\&#39;/g')
    random=$(mpc | grep -Eo 'random:[[:space:]]+on')
    echo "$song${random:+ <span size=\"small\" foreground=\"#${__BASE0E}\">R</span>}"
fi

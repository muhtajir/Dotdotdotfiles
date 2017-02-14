#!/bin/bash

if [[ $BLOCK_BUTTON = 1 ]]; then
    playerctl -p ${BLOCK_INSTANCE} play-pause
fi

sleep .5
artist=$(playerctl -p ${BLOCK_INSTANCE} metadata artist)
title=$(playerctl -p ${BLOCK_INSTANCE} metadata title)

if [[ $artist = '(null)' ]]; then
    echo '<i>silence…</i>'
else
    # escape pango (=html) special chars
    artist=$(echo $artist | sed 's/&/\&amp;/g; s/</\&lt;/g; s/>/\&gt;/g')
    title=$(echo $title | sed 's/&/\&amp;/g; s/</\&lt;/g; s/>/\&gt;/g')
    echo "<b>${artist}</b> - ${title}"
fi

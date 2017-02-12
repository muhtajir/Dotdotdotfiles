#!/bin/bash

sleep .5
artist=$(playerctl metadata -p ${BLOCK_INSTANCE} artist)
title=$(playerctl metadata -p ${BLOCK_INSTANCE} title)

if [[ $artist ]]; then
    echo '<i>silence…</i>'
else
    # escape pango (=html) special chars
    artist=$(echo $artist | sed 's/&/\&amp;/g; s/</\&lt;/g; s/>/\&gt;/g')
    title=$(echo $title | sed 's/&/\&amp;/g; s/</\&lt;/g; s/>/\&gt;/g')
    echo "<b>${artist}</b> - ${title}"
fi

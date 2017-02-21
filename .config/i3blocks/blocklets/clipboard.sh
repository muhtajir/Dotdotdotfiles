#!/bin/bash

# This script displays the contents of the clipboard, cut down to the first 20
# characters. If the clipboard is empty, nothing is shown. Whenever the
# blocklet is clicked (with any mouse button), the clipboard is emptied and the
# blocklet disappears.

# TODO adapt to xsel or xput

if [[ -n ${BLOCK_BUTTON} ]]; then
    xsel -bc
else
    clipcontent=$(xsel -bo)
    [[ -z $clipcontent ]] && exit
    # format string and escape pango (=html) special chars
    clipcontent=$(echo $clipcontent | head -n 1)
    clipcontent=$(echo ${clipcontent:0:20} | sed 's/&/\&amp;/g; s/</\&lt;/g; s/>/\&gt;/g')
    echo "<span font_size='small'>${clipcontent}…</span>"
fi

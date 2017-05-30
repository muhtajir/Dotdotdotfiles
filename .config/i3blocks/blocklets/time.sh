#!/bin/bash

if [[ -z "${BLOCK_BUTTON}" ]]; then
    echo -n $(date +%H:%M)
else
    notify-send "$(date +%A,\ %d.%m.%Y)"
fi
# echo -n '</b>'

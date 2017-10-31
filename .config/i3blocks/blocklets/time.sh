#!/bin/bash

echo -n $(date +%H:%M)

if [[ ${BLOCK_BUTTON} -eq 1 ]]; then
    echo
    notify-send "$(date +%A,\ %d.%m.%Y)" "$(cal)"
fi

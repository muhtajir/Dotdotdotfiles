#!/bin/bash

if [[ -z "${BLOCK_BUTTON}" ]]; then
    echo -n $(date +%H:%M)
else
    echo -n $(date +%H:%M)
    notify-send "$(date +%A,\ %d.%m.%Y)"
fi

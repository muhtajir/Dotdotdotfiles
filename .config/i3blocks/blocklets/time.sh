#!/bin/bash

echo -n $(date +%H:%M)

if [[ ${BLOCK_BUTTON} -eq 1 ]]; then
    notify-send --icon=none "$(date +%A,\ %d.%m.%Y)" "\n$(cal)"
fi

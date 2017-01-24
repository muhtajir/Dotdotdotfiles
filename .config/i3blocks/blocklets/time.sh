#!/bin/bash

if [[ -z "${BLOCK_BUTTON}" ]]; then
    echo $(date +%H:%M)
else
    echo $(date +%d.%m.%y)
    # pkill -RTMIN+25 i3blocks
    # exit
fi


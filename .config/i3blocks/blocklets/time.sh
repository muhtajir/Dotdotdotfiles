#!/bin/bash

if [[ -z "${BLOCK_BUTTON}" ]]; then
    echo -n $(date +%H:%M)
else
    echo -n $(date +%d.%m.%y)
    # pkill -RTMIN+25 i3blocks
    # exit
fi
# echo -n '</b>'

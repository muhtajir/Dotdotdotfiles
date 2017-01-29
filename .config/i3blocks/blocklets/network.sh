#!/bin/bash

state=$(cat /sys/class/net/${BLOCK_INSTANCE}/operstate)
if [[ $state == "up" ]]; then
    ping 8.8.8.8 -c 1 -W 3 &>/dev/null
    if [[ $? == 0 ]]; then
        echo  '<span foreground="#8ca06b"></span>'
    else
        echo '<span foreground="#fdcc59"></span>'
    fi
else
    echo '<span foreground="#dd464c"></span>'
fi

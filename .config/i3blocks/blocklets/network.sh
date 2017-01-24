#!/bin/bash

state=$(cat /sys/class/net/${BLOCK_INSTANCE}/operstate)
if [[ $state == "up" ]]; then
    echo  '<span foreground="#8ca06b"></span>'
else
    echo "<span foreground="#dd464c"></span>"
fi

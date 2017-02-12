#!/bin/bash


# wired connection
WIRED_INSTANCE=${BLOCK_INSTANCE%%;*}
STATE=$(cat /sys/class/net/${WIRED_INSTANCE}/operstate)
if [[ $STATE == "up" ]]; then
    if [[ $(ip route | grep ${WIRED_INSTANCE}) ]]; then
        wired='<span foreground="#8ca06b"></span>'
    else
        wired='<span foreground="#fdcc59"></span>'
    fi
fi

# wifi connection
WIFI_INSTANCE=${BLOCK_INSTANCE##*;}
INTERFACE="${WIFI_INSTANCE:-wlan0}"
QUALITY=$(grep $INTERFACE /proc/net/wireless | awk '{ print int($3 * 100 / 70) }')
WIFI_SSID=$(iwgetid -r)
GEN_STATE=$(ip link show ${WIFI_INSTANCE} | grep -Eo 'state\s\w+')

# stop here if wifi device doesn't exist or it's in 'down' state
if [[ ${GEN_STATE##state } = 'DOWN' || $(ip link show ${WIFI_INSTANCE}) ]]; then
    echo $wired
    exit
fi

# color
if [[ $QUALITY -ge 80 ]]; then
    mark_front='<span foreground="#8ca06b">'
elif [[ $QUALITY -lt 80 ]]; then
    mark_front='<span foreground="#fdcc59">'
elif [[ $QUALITY -lt 40 ]]; then
    mark_front='<span foreground="#dd464c">'
fi
wifi="${mark_front}</span>"

echo "$wired $wifi ${WIFI_SSID}" # full text
echo "$wired $wifi" # short text

#!/bin/bash

source $(dirname $(realpath $0))/colors

WIRED_INSTANCE=${BLOCK_INSTANCE/;*/}
WIFI_INSTANCE=${BLOCK_INSTANCE/*;/}

# wired connection
if [[ -n $WIRED_INSTANCE ]]; then
    STATE=$(cat /sys/class/net/${WIRED_INSTANCE}/operstate)

    if [[ $STATE == "up" ]]; then
        if [[ $(ip route | grep ${WIRED_INSTANCE}) ]]; then
            wired="<span foreground=\"${color01}\"></span>"
        else
            wired="<span foreground=\"${color02}\"></span>"
        fi
    fi
fi

# wifi connection
if [[ -n $WIFI_INSTANCE ]]; then
    if [[ ! $(cat /sys/class/net/${WIFI_INSTANCE}/operstate) = 'up' ]];then
    wifi=''
    else
        QUALITY=$(grep $WIFI_INSTANCE /proc/net/wireless | awk '{ print int($3 * 100 / 70) }')
        WIFI_SSID=$(iwgetid -r)

        if [[ $QUALITY -ge 80 ]]; then
            mark_front="<span foreground=\"${color01}\">"
        elif [[ $QUALITY -ge 60 ]]; then
            mark_front="<span foreground=\"${color02}\">"
        elif [[ $QUALITY -ge 40 ]]; then
            mark_front="<span foreground=\"${color03}\">"
        else
            mark_front="<span foreground=\"${color04}\">"
        fi
        wifi="${mark_front}</span>"
    fi
fi

# full text
echo "${wired}${wifi:+${wired:+ }$wifi${WIFI_SSID:+ <span size='small'>${WIFI_SSID}</span>}}"
# short text
echo "${wired}${wifi:+${wired:+ }$wifi}"

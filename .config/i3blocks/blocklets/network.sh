#!/bin/bash

if [[ $BLOCK_BUTTON = 1 ]]; then
    $HOME/.config/rofi/custom_modes/nm_wifi_dmenu.fish
fi

WIRED_INSTANCE=${BLOCK_INSTANCE/;*/}
WIFI_INSTANCE=${BLOCK_INSTANCE/*;/}

# wired connection
if [[ -n $WIRED_INSTANCE ]]; then
    STATE=$(cat /sys/class/net/${WIRED_INSTANCE}/operstate)

    if [[ $STATE == "up" ]]; then
        if [[ $(ip route | grep ${WIRED_INSTANCE}) ]]; then
            wired="<span foreground=\"#${__BASE0B}\"></span>"
        else
            wired="<span foreground=\"#${__BASE0A}\"></span>"
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
            mark_front="<span foreground=\"#${__BASE0B}\">"
        elif [[ $QUALITY -ge 60 ]]; then
            mark_front="<span foreground=\"#${__BASE0A}\">"
        elif [[ $QUALITY -ge 40 ]]; then
            mark_front="<span foreground=\"#${__BASE09}\">"
        else
            mark_front="<span foreground=\"#${__BASE08}\">"
        fi
        wifi="${mark_front}</span>"
    fi
fi

# simple test for VPN
systemctl is-active openvpn-client@client > /dev/null &&
    vpn="<span foreground=\"#${__BASE08}\" size='x-small'>VPN</span>"

# full text
echo "${wired}${wifi:+${wired:+ }$wifi${WIFI_SSID:+ <span size='small'>${WIFI_SSID}</span>}}${vpn:+ }$vpn"
# short text
echo "${wired}${wifi:+${wired:+ }$wifi}"

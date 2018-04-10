#!/usr/bin/env fish

set RESCAN_STRING "Rescan Networks"
set CONN_STRING "Connection established"
set SEC_CONN_STRING "Connected to"
set ERR_STRING "Error"
set SEC_ERR_STRING "Error connecting to"
set ENTER_STRING "Enter password for WIFI network"

printf '%s (%s) %s\n' (
    nmcli -e no -g SSID,BSSID,SECURITY device wifi list \
    | string split -r -m 1 ':' \
    | string split -m 1 ':'
    ) | while read line
        set wifi_list $wifi_list $line
    end

printf '%s\n' "$RESCAN_STRING" $wifi_list | dmenu -i -p wifi | read sel
    or exit

if [ "$sel" =  "$RESCAN_STRING" ]
    nmcli device wifi rescan
else
    set parsed (string match -r '(.+?)\s+\(([A-F0-9:]+)\)([^\(]+)$' $sel)
    set ssid $parsed[2]
    set bssid $parsed[3]
    set sec $parsed[4]

    if string match -qr '(WPA|WEP)' $sec
        set -l pass (
            printf '%s\n' "SETPROMPT $ENTER_STRING $ssid: " "GETPIN" | pinentry | string match -r '(?<=^D ).+'
            )
        nmcli device wifi connect "$bssid" password "$pass"
            and notify-send "$CONN_STRING" "$SEC_CONN_STRING $ssid"
            or notify-send "$ERR_STRING" "$SEC_ERR_STRING $ssid"
    else
        nmcli device wifi connect "$bssid"
            and notify-send "$CONN_STRING" "$SEC_CONN_STRING $ssid"
            or notify-send "$ERR_STRING" "$SEC_ERR_STRING $ssid"
    end
end

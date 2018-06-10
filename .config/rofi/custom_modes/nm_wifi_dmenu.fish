#!/usr/bin/env fish

set RESCAN_STRING "Rescan Networks"
set CONN_STRING "Connection established"
set SEC_CONN_STRING "Connected to"
set ERR_STRING "Error"
set SEC_ERR_STRING "Error connecting to"
set ENTER_STRING "Enter password for WIFI network"

function success_feedback -a exit_code
    if [ $exit_code -eq 0 ]
        notify-send "$CONN_STRING" "$SEC_CONN_STRING $ssid"
    else
        notify-send "$ERR_STRING" "$SEC_ERR_STRING $ssid"
    end
    exit
end

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

    # first try to activate the connection profile in case a connection has been previously established
    nmcli connection up id "$ssid"
    if [ $status -eq 0 ]
        success_feedback 0
    end

    if string match -qr '(WPA|WEP)' $sec
        set -l pass (
            printf '%s\n' "SETPROMPT $ENTER_STRING $ssid: " "GETPIN" | pinentry | string match -r '(?<=^D ).+'
            )
        nmcli device wifi connect "$bssid" password (string escape "$pass")
        success_feedback $status
    else
        nmcli device wifi connect "$bssid"
        success_feedback $status
    end
end

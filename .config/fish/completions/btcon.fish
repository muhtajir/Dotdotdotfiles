set PROG 'btcon'

complete -c $PROG -f

if systemctl is-active bluetooth.service
    for line in (bluetoothctl devices)
        set mac (string match -r '\b[0-9A-F:]+\b' $line)
        set dev (string match -r '(?<=[0-9A-F]{2}\s).+$' $line)
        complete -c $PROG -a $mac -d $dev
    end
end

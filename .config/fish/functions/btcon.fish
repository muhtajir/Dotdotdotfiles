#!/usr/bin/env fish

function btcon
    if not systemctl is-active bluetooth.service > /dev/null
        sudo systemctl start bluetooth
    end

    bluetoothctl power on 2>/dev/null 1>/dev/null

    if [ -z "$argv" ]
        echo "Specify device to connect to:"
        bluetoothctl devices 2>/dev/null
        return 1
    end

    bluetoothctl connect "$argv"
end

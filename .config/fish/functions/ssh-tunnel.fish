function ssh-tunnel
    test -n "$argv"; or set argv 4711

    ssh-add $HOME/.ssh/cloud
    # hacky
    begin
        set -lx _flag_value $argv
        _validate_int --min 1024 --max 65535 > /dev/null
        or begin
            echo 'Provide valid integer within allowed port range.'
            return
        end
    end

    begin
        ssh -TND $argv do.cloud > /dev/null &
        disown
    end
    and echo "SSH tunnel opened at port $argv."
end

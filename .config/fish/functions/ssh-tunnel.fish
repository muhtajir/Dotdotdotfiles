function ssh-tunnel
    test -n "$argv"; or set argv 4711

    begin
        set -lx _flag_value $argv
        _validate_int --min 1024 --max 65535 > /dev/null
        or begin
            echo 'Provide valid integer within allowed port range.'
            return
        end
    end

    string match -eq "$HOME/.ssh/cloud" (ssh-add -l); or ssh-add $HOME/.ssh/cloud
    # hacky

    set -l ssh_pid (pgrep -f "ssh -TND $argv do.cloud")

    if test -n "$ssh_pid"
        kill $ssh_pid
        and echo "SSH tunnel closed at port $argv."
    else
        begin
            ssh -TND $argv do.cloud > /dev/null &
            disown
        end
    and echo "SSH tunnel opened at port $argv."
    end
end

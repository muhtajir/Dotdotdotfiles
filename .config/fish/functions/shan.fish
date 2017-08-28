# lots and lots to do before this will work
function shan
    set -l gio_out (gio mount -li)
    set -l counter 0

    set -l pos 1
    set -l match_pos
    for line in (gio mount -li)
        if string match -q "Device($counter)*" $line
            break
            set match_pos $pos
        end
    end

    if test -n "$match_pos"
        for line in $gio_out[$match_pos..-1]
            if string match -q "Device*" $line
                break
            end
        end
    end

end


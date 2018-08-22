function fish_vi_cursor --on-variable fish_bind_mode
    # only xterm or this is just gonna cause countless headaches
    if not string match -q 'xterm-*' $TERM
        return
    end

    if [ $fish_bind_mode = "insert" ]
        echo -ne '\033[0 q'
    else
        echo -ne '\033[1 q'
    end
            
end

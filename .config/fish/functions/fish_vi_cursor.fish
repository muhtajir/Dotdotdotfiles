function fish_vi_cursor --on-variable fish_bind_mode --on-event fish_prompt
    # only termite or this is just gonna cause countless headaches
    # if not string match -q -- '*-termite' $TERM
    #     return
    # end

    if [ $fish_bind_mode = "insert" ]
        echo -ne '\033[0 q'
    else
        echo -ne '\033[1 q'
    end
            
end

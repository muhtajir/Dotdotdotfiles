function fish_prompt
    if string match -q 'xterm-*' $TERM
        __fish_create_first_prompt_seg
        __fish_create_second_prompt_seg
    else
        echo (prompt_pwd)'$ '
    end
end

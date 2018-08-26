function fish_prompt
    # catch status before if statement
    set -l return_code $status
    if string match -q 'xterm-*' $TERM
        # pass status to first prompt segment
        __fish_create_first_prompt_seg $return_code
        __fish_create_second_prompt_seg
    else
        echo (prompt_pwd)'$ '
    end
end

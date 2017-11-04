function __fish_create_first_prompt_seg --description 'Put together first prompt segment'
    # catch the exit status before anything else or it will be lost
    set -l return_code $status

    # now choose content for the first prompt segment
    # highest ranking output is bind mode
    if test $fish_bind_mode != insert
        __fish_draw_first_prompt_seg yellow " Φ "
    # second is last exit code
    else if test $return_code -gt 0
        __fish_draw_first_prompt_seg red " $return_code "
    # least important is active jobs
    else if jobs > /dev/null 2> /dev/null
        set -l fish_last_job (jobs -lc | tail -n 1)
        __fish_draw_first_prompt_seg cyan " $fish_last_job "
    # and after that it's just a percent sign
    else
        __fish_draw_first_prompt_seg blue ' %% '
    end
end

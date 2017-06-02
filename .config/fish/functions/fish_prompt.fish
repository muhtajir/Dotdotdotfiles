function fish_prompt --description 'Write out the prompt'
    create_first_seg
    second_seg $color02 (basename (prompt_pwd))
end

function create_first_seg --description 'Put together first segment'
    # catch the exit status before anything else or it will be lost
    set -l return_code $status

    # now choose content for the first segment
    # highest ranking output is bind mode
    if test $fish_bind_mode != insert
        draw_first_seg $color16 " Φ "
    # second is last exit code
    else if test $return_code -gt 0
        draw_first_seg $color01 " $return_code "
    # least important is active jobs
    else if jobs > /dev/null ^ /dev/null
        set -l fish_last_job (jobs -lc | tail -n 1)
        draw_first_seg $color06 " $fish_last_job "
    # and after that it's just a percent sign
    else
        draw_first_seg $color04 ' %% '
    end
end

function draw_first_seg -a bg_color content
    chcol $bg_color; and printf '░▒▓'
    chcol $color18 $bg_color ; and printf $content
    set -g fish_color_prompt_first_seg_bg $bg_color
end

function second_seg -a bg_color content
    chcol $fish_color_prompt_first_seg_bg $bg_color; and printf ''
    chcol $color18; and printf ' %s ' $content
    chcol $bg_color normal; and printf ' '
    chcol normal normal
end

function chcol -a fg bg --description 'Change foreground and optionally background color'
    if test -n "$bg"
        set_color -b $bg $fg
    else
        set_color $fg
    end
end

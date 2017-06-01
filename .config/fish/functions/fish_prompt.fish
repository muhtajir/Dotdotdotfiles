function fish_prompt --description 'Write out the prompt'
    create_first_seg
    second_seg $color02 (prompt_pwd)
    # chcol $color04; and printf '░▒▓'
    # chcol $color18 $color04 ; and printf ' fish '
    # chcol $color04 $color02; and printf ''
    # chcol $color18; and printf ' %s ' (prompt_pwd)
    # chcol $color02 normal; and printf ' '
    # chcol normal normal
end

function create_first_seg --description 'Put together first segment'
    set -l return_code $status
    if test $return_code -gt 0
        draw_first_seg $color01 " $return_code "
    else
        draw_first_seg $color04 ' fish '
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

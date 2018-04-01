function __fish_draw_first_prompt_seg -a bg_color content
    __fish_chcol $bg_color; and printf '░▒▓'
    __fish_chcol $__BASE00 $bg_color ; and printf $content
    set -g fish_color_prompt_first_seg_bg $bg_color
end

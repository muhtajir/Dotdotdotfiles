function fish_prompt --description 'Write out the prompt'
    chcol $color04; and printf '░▒▓'
    chcol $color18 $color04 ; and printf ' %s ' (prompt_pwd)
    chcol $color04 normal; and printf ' ' 
    chcol normal normal
end

function chcol -a fg bg
    if test -n "$bg"
        set_color -b $bg $fg
    else
        set_color $fg
    end
end

function fish_prompt --description 'Write out the prompt'
    set_color $color04; and printf '░▒▓'
    set_color -b $color04 ; and set_color $color18; and printf ' %s ' (prompt_pwd)
    set_color -b normal; set_color $color04; and printf ' ' 
    set_color -b normal normal
end

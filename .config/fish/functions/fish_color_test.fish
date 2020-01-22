function fish_color_test -d 'Print the fish colors'
    set_color $fish_color_normal; echo fish_color_normal; set_color normal
    set_color $fish_color_command; echo fish_color_command; set_color normal
    set_color $fish_color_quote; echo fish_color_quote; set_color normal
    set_color $fish_color_redirection; echo fish_color_redirection; set_color normal
    set_color $fish_color_end; echo fish_color_end; set_color normal
    set_color $fish_color_error; echo fish_color_error; set_color normal
    set_color $fish_color_param; echo fish_color_param; set_color normal
    set_color $fish_color_comment; echo fish_color_comment; set_color normal
    set_color $fish_color_match; echo fish_color_match; set_color normal
    set_color $fish_color_selection; echo fish_color_selection; set_color normal
    set_color $fish_color_search_match; echo fish_color_search_match; set_color normal
    set_color $fish_color_operator; echo fish_color_operator; set_color normal
    set_color $fish_color_escape; echo fish_color_escape; set_color normal
    set_color $fish_color_autosuggestion; echo fish_color_autosuggestion; set_color normal
    set_color $fish_color_cancel; echo fish_color_cancel; set_color normal

    set_color $fish_pager_color_prefix; echo fish_pager_color_prefix; set_color normal
    set_color $fish_pager_color_completion; echo fish_pager_color_completion; set_color normal
    set_color $fish_pager_color_description; echo fish_pager_color_description; set_color normal
    set_color $fish_pager_color_progress; echo fish_pager_color_progress; set_color normal
    set_color $fish_pager_color_secondary; echo fish_pager_color_secondary; set_color normal
end

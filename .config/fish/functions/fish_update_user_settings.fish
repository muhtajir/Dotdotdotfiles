function fish_update_user_settings
    set -l uvars
    set -l uvars_all (set -Un)
    set -l uvar_native __fish_init_2_39_8 \
        __fish_init_2_3_0 \
        fish_color_autosuggestion \
        fish_color_command \
        fish_color_comment \
        fish_color_cwd \
        fish_color_cwd_root \
        fish_color_end \
        fish_color_error \
        fish_color_escape \
        fish_color_history_current \
        fish_color_host \
        fish_color_match \
        fish_color_normal \
        fish_color_operator \
        fish_color_param \
        fish_color_quote \
        fish_color_redirection \
        fish_color_search_match \
        fish_color_selection \
        fish_color_user \
        fish_color_valid_path \
        fish_greeting \
        fish_key_bindings \
        fish_pager_color_completion \
        fish_pager_color_description \
        fish_pager_color_prefix \
        fish_pager_color_progress 

    # only erase non-native variables
    for var in uvars_all
        set -l match_native (string match "*$var*" $uvar_native)
        if test -z "$match_native"
            set uvars $uvars $var
        end
    end

    for var in $uvars
        set -e $var
    end

    fish_set_universal_variables

    set -l usrabbr (abbr -l)
    for abbrev in $usrabbr
        abbr -e $abbrev
    end

    fish_set_user_abbreviations
    # this last one is redundant for now, as all aliases are set at shell
    # startup
    # fish_set_user_aliases
end

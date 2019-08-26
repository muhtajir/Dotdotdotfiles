function fish_update_user_settings
    __fish_set_universal_variables

    set -l usrabbr (abbr -l)
    for abbrev in $usrabbr
        abbr -e $abbrev
    end

    __fish_set_user_abbreviations
end

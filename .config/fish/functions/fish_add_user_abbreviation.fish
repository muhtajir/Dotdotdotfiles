function fish_add_user_abbreviation
    if test (count $argv) -eq 0
        abbr
        return
    end

    set -l abbr_file ~/.config/fish/functions/__fish_set_user_abbreviations.fish
    sed -ri '/^end$/d' $abbr_file
    echo "    abbr --add $argv" >> $abbr_file
    echo "end" >> $abbr_file
    __fish_set_user_abbreviations
end

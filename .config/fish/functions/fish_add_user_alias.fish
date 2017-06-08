function fish_add_user_alias
    set -l abbr_file ~/.config/fish/functions/fish_set_user_aliases.fish
    sed -ri '/^end$/d' $abbr_file
    echo "    alias $argv" >> $abbr_file
    echo "end" >> $abbr_file
    fish_set_user_aliases
end

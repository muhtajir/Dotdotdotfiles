function fish_add_user_alias
    if test (count $argv) -eq 0
        alias
        return
    end

    set -l abbr_file ~/.config/fish/functions/__fish_set_user_aliases.fish
    sed -ri '/^end$/d' $abbr_file
    echo "    alias $argv" >> $abbr_file
    echo "end" >> $abbr_file
    __fish_set_user_aliases
end

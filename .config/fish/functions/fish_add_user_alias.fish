function fish_add_user_alias
    if test (count $argv) -eq 0
        alias
        return
    end

    set -l alias_file ~/.config/fish/functions/__fish_set_user_aliases.fish
    set aliases (string match -re '^(?!function|end$)' < $alias_file)
    set -a aliases "    alias $argv"
    set aliases (string join0 $aliases | sort -z | string split0)
    set -a aliases "end"
    set -p aliases "function __fish_set_user_aliases"
    string join \n $aliases > $alias_file
    __fish_set_user_aliases
end

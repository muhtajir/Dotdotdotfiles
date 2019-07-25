function fish_add_user_abbreviation
    if test (count $argv) -eq 0
        abbr
        return
    end

    set -l abbr_file ~/.config/fish/functions/__fish_set_user_abbreviations.fish
    set abbrs (string match -re '^(?!function|end$)' < $abbr_file)
    set -a abbrs "    abbr --add $argv"
    set abbrs (string join0 $abbrs | sort -z | string split0)
    set -a abbrs "end"
    set -p abbrs "function __fish_set_user_abbreviations"
    string join \n $abbrs > $abbr_file
    __fish_set_user_abbreviations
end

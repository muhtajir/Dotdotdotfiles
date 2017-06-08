function fish_add_universal_variable
	set -l abbr_file ~/.config/fish/functions/fish_set_universal_variables.fish
    sed -ri '/^end$/d' $abbr_file
    echo "    set -U $argv" >> $abbr_file
    echo "end" >> $abbr_file
    fish_set_universal_variables
end

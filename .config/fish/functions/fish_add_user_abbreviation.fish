function fish_add_user_abbreviation
	set -l abbr_file ~/.config/fish/functions/fish_set_user_abbreviations.fish
    sed -ri '/^end$/d' $abbr_file
    echo "    abbr --add $argv" >> $abbr_file
    echo "end" >> $abbr_file
    fish_set_user_abbreviations
end

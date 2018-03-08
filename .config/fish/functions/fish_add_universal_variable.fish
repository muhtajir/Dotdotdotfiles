function fish_add_universal_variable
    if test (count $argv) -eq 0
        set -U
        return
    end

	set -l abbr_file ~/.config/fish/functions/__fish_set_universal_variables.fish
	set -l abbr_skel_file ~/.config/fish/functions/__fish_set_universal_variables.fish
    sed -ri '/^end$/d' $abbr_file
    sed -ri '/^end$/d' $abbr_skel_file
    echo "    set -U $argv" >> $abbr_file
    echo "    set -U $argv" >> $abbr_skel_file
    echo "end" >> $abbr_file
    echo "end" >> $abbr_skel_file
    __fish_set_universal_variables
end

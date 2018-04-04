#!/usr/bin/env fish

set options 'Append: ' 'Append & Play: ' 'Replace: ' 'Replace & Play: '

function is_option -a entry
    for opt in $options
        string match -q "$opt*" $entry; and return 0
    end
    # for-else
    return 1
end

function parse_entry -a entry
    set cmd
    set pl

    for opt in $options
        if string match -q "$opt*" "$entry"
            set cmd $opt
            # this is safe because it only replaces the first occurence
            set pl (string replace "$opt" '' "$entry")
        end
    end

    switch $cmd
        case $options[1]
            mpc load $pl >&2
        case $options[2]
            set pos (count (mpc playlist)) >&2
            set pos (math $pos + 1) >&2
            mpc load $pl >&2
            mpc play $pos >&2
        case $options[3]
            mpc clear >&2
            mpc load $pl >&2
        case $options[4]
            mpc clear >&2
            mpc load $pl >&2
            mpc play >&2
    end
end

if [ -z "$argv" ]
    mpc lsplaylists
else if not is_option "$argv"
    set pl "$argv"
    printf "%s$pl\n" $options
else if is_option "$argv"
    parse_entry "$argv"
end

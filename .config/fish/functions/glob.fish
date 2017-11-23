function glob
	set -l first_param_char (string sub -s 1 -l 1 -- $argv[1])
    set -l mode
	if test "$first_param_char" = '-'
        set mode (string sub -s 2 -- $argv[1])
    else
        return
    end

    # include a check for a hidden switch at some point
    set -l hidden -not -name '.*'
    # save non-option arguments in $args
    set -l args
    if test (count $argv) -ge 2
        set args $argv[2..(count $argv)]
    end

    switch $mode
        case 'x'
            # match all except args
            set -l params
            for arg in $args
                set params $params -not -name {$arg}
            end
            find . -maxdepth 1 $params $hidden
        case 'd'
            # only match directories
            find . -maxdepth 1 -type d $hidden
        case 'f'
            # only match files
            find . -maxdepth 1 -type f $hidden
        case '*'
            return
    end
end

#!/usr/bin/env fish

if [ -z "$argv" ]
    set num 0
    for track in (mpc playlist)
        set num (math $num + 1)
        printf '%d: %s\n' $num "$track"
    end
else
    set pos (string match -r '^\d+' $argv)
    mpc play $pos >&2
end

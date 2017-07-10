#!/usr/bin/env fish

# get name of currently active window
set -l active_line (string match -ri '^_NET_ACTIVE_WINDOW\(WINDOW\).+' (xprop -root))
set -l active_id (string replace -r '^[^#]+#\s(\S+)' '$1' $active_line)
set -l name_line (string match -r 'WM_NAME\([^)]+\).+' (xprop -id $active_id))
set -l name (string match -r '"[^"]+"' $name_line)
set name (string replace -a ' ' '_' $name)

# get list of all pass files
set -l passes (find ~/.password-store -name '*.gpg')

# loop through files and find one that matches
set -l candidates
for passfile in $passes
    set -l barepass (command basename -s '.gpg' $passfile)
    string match -qi "*$barepass*" $name
    if test $status -eq 0
        set candidates $candidates $passfile
    end
end

# send candidate to pass; use dmenu to select one if there are too many
set -l cand_num (count $candidates)
if test $cand_num -eq 1
    set -l pass_friendly (string replace -r '^.+?\.password-store/(.+?)\.gpg$' '$1' $candidates)

    notify-send "Found $pass_friendly"
    pass -c $pass_friendly
else if test $cand_num -gt 1
    notify-send 'Too many candidates.'
else
    notify-send 'Nothing found.'
end


# for passfile in (find ~/.password-store -name '*.gpg')
#     # set -l passname (command basename $passfile | string replace -r '.gpg$' '')
#     set passes $passes $passfile
# end

# echo $name
# echo $passes

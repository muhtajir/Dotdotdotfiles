#!/usr/bin/env fish

test (count $argv) -eq 1; and test $argv[1] = '-l'; and set -l login_mode
test -z "$PASS_SUBNAME_SPLIT_CHAR"; and set -l PASS_SUBNAME_SPLIT_CHAR '%'

# get name of currently active window
set -l active_line (string match -ri '^_NET_ACTIVE_WINDOW\(WINDOW\).+' (xprop -root))
set -l active_id (string replace -r '^[^#]+#\s(\S+)' '$1' $active_line)
set -l name_line (string match -r 'WM_NAME\([^)]+\).+' (xprop -id $active_id))
set -l name (string match -r '".+?"$' $name_line)
set name (string replace -a ' ' '_' $name)

# get list of all pass files
set -l passes (string replace -r '^.+?\.password-store/(.+?)\.gpg$' '$1' $HOME/.password-store/**/*)

# loop through files and find one that matches
set -l candidates
for password in $passes
    set -l barepass (command basename $password)
    string match -qi "*$barepass*" $name
    if test $status -eq 0
        set candidates $candidates $password
    end
end

# if there are no matches try secondary names
if test -z "$candidates"
    for password in $passes
        set -l barepass (command basename $password)
        set -l subs (string split "$PASS_SUBNAME_SPLIT_CHAR" "$barepass")
        test $status -eq 0; and for sub in $subs
            echo $sub
            echo $name
            string match -qi "*$sub*" $name
            if test $status -eq 0
                set candidates $password
            end
        end
    end
end

echo $candidates

# send candidate to pass; use dmenu to select one if there are too many
set -l cand_num (count $candidates)
if test $cand_num -eq 1
    notify-send "Found $candidates"

    if set -q login_mode
        pass show $candidates | string replace -rf '^login:\s?(.+)$' '$1' | xclip -selection clipboard -i
    else
        pass show -c $candidates
    end
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

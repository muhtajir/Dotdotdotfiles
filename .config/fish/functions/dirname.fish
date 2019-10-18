function dirname -d 'Remove trailing filename from path'
    [ -z "$argv" ] && return 1
    if [ (string sub -s -1 "$argv") = "/" ]
        echo "$argv"
    else
        command dirname "$argv"
    end
end

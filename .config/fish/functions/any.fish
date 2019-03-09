function any -d 'Return true if any tokens are passed as arguments'
    if [ (count $argv) -gt 0 ]
        true
    else
        false
    end
end

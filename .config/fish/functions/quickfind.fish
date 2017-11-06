function quickfind -d 'Do a simple find operation in current directory'
    find . -iname '*'"$argv"'*'
end

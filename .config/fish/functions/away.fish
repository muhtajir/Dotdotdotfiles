function away -d 'Run a command and immediately disown it'
    fish -c $argv >/dev/null 2>&1 &
    disown
end

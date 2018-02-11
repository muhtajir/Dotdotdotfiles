function away -d 'Run a command and immediately disown it'
    fish -c (string escape $argv) >/dev/null 2>&1 &
    disown
end

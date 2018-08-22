function away -d 'Run a command and immediately disown it'
    set -l cmd (string escape -- $argv)
    fish -c "$cmd" >/dev/null 2>&1 &
    disown
end

function away -d 'Run a command and immediately disown it'
    fish -c (string escape $argv) &
    set -l job (jobs -lp)
    disown $job
end

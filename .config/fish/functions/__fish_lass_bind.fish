function __fish_lass_bind
    # operate either on the current commandline or the last history entry if the
    # current commandline is empty
    set -l cmdln_old (commandline)
    if [ (commandline) = '' ]
        set cmdln_old (history | head -n 1)
    end

    # if the selected entry already has the keyword prepended, don't do any
    # further modification
    if not string match -rq '\|\s*less$' "$cmdln_old" 
        set -l cursor_pos (commandline --cursor)
        set -l cmdln_new "$cmdln_old"' | less'
        commandline -r $cmdln_new
        commandline --cursor $cursor_pos
    else
        commandline -r $cmdln_old
    end
    sleep .1

    commandline -f execute
end

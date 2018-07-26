function __fish_plz_bind
    set -l cmdln_old (commandline)
    if [ (commandline) = '' ]
        set cmdln_old (history | head -n 1)
    end

    if [ ! (string sub -l 5 "$cmdln_old") = "sudo " ]
        set -l cursor_pos (commandline --cursor)
        set -l cmdln_new 'sudo '"$cmdln_old"
        commandline -r $cmdln_new
        commandline --cursor $cursor_pos
        sleep .1
    end

    commandline -f execute
end

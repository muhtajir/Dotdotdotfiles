function __fish_away_bind
    set -l cursor_pos (commandline --cursor)
    set -l cmdln (commandline)

    if [ ! (string sub -l 5 "$cmdln") = "away " ]
        set -l cmdln 'away '(commandline)
        commandline -r $cmdln
        commandline --cursor $cursor_pos
        sleep .1
    end
    commandline -f execute
end

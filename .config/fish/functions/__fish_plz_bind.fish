function __fish_plz_bind
    set -l cursor_pos (commandline --cursor)
    set -l cmdln 'sudo '(commandline)
    commandline -r $cmdln
    commandline --cursor $cursor_pos
    sleep .1
    commandline -f execute
end

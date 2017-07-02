function fish_plz_bind --description 'Prepend sudo to current line and run it'
    set -l cursor_pos (commandline --cursor)
	set -l cmdln 'sudo '(commandline)
    commandline -r $cmdln
    commandline --cursor $cursor_pos
    sleep .1
    commandline -f execute
end

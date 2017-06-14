function fish_plz_bind --description 'Prepend sudo to current line and run it'
	set -l cmdln (commandline)
    commandline -r ''
    printf \n
    eval "sudo $cmdln"
    commandline -f repaint
end

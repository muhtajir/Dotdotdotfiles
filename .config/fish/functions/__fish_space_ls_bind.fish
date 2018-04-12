function __fish_space_ls_bind
    # broken for now
    if [ -z (commandline) ]
        commandline -f force-repaint
        echo
        ls
    else
        commandline -i ' '
    end
end

function __fish_vifm_bind -d 'Open vifm in CWD'
    if test (commandline) = ''
        vifm -c only --select (pwd)
    else
        set -l start_dir
        set -l cmd_token (commandline -t)

        # expand tilde if necessary
        set cmd_token (string replace -r '^~' $HOME $cmd_token)

        if test -e $cmd_token
            set start_dir $cmd_token
        else if test -e (dirname $cmd_token)
            set start_dir (dirname $cmd_token)
        else
            set start_dir (pwd)
        end
        echo $cmd_token
        echo $start_dir

        set -l files
        vifm -c only --select $start_dir --choose-files - | while read -l file
            set files $files (string escape $file)
        end

        set files (string join ' ' $files)
        if test -n "$files"
            commandline -a $files

            # move cursor to final position
            set pos (string length (commandline))
            commandline -C $pos
        end
    end

    commandline -f repaint
end

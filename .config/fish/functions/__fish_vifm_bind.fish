function __fish_vifm_bind -d 'Open vifm in CWD'
    if test (commandline) = ''
        vifm -c only --select (pwd)
    else
        set -l files
        vifm -c only --select (pwd) --choose-files - | while read -l file
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

function sudo --description 'A wrapper for sudo that looks up aliases if the specified command is not in $PATH'
    set -l arg_num (count $argv)
    set -l command
    set -l args
    if test $arg_num -eq 0
        command sudo
        return
    else if test $arg_num -eq 1
        set command $argv[1]
    else
        set command $argv[1]
        set args $argv[2..-1]
    end

    set -l which (command -s $command)
    if test -z $which
        set -l new_command

        # loop through aliases and try to get a match for $command
        for al in (alias ^ /dev/null)
            set new_command (string replace -fr '^alias\s'$command'(=|\s)(.+)$' '$2' $al)
            test -n "$new_command"; and break

        end

        set command $new_command
    end

    # finally run the command + args with sudo
    eval "command sudo $command $args"
end

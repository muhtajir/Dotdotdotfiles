function sudo --description 'A wrapper for sudo that looks up aliases if the specified command is not in $PATH'
    set -l arg_num (count $argv)
    set -l command
    set -l args

    if test $arg_num -eq 0 -o (string sub -l 1 -- "$argv") = '-'
        command sudo $argv
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
        for al in (alias 2> /dev/null)
            string replace -fr '^alias\s'$command'(=|\s)(.+)$' '$2' $al | read -a new_command
            test -n "$new_command"; and break

        end

        set command $new_command
    end

    # finally run the command + args with sudo
    command sudo $command $args
end

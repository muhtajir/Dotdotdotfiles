function pre --description 'Bash style local variable prefixing'
    set vars
    set cmd
    set vars_allowed 0

    for a in $argv
        if string match -rq '^[^=]+=[^=]+$' "$a"; and [ $vars_allowed -eq 0 ]
            set vars $vars $a
        else
            set vars_allowed 1
            set cmd $cmd $a
        end
    end

    for v in $vars
        set parts (string split '=' "$v")
        set -x $parts[1] $parts[2]
    end
    eval $cmd

end

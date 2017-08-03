function pass_create

    set -l args (count $argv)
    switch $args
        case 0
            echo "Missing name for new password file."
            return
        case 1
            # continue
        case '*'
            echo "Too many arguments."
            return
    end

    read -P "login (none by default): " -l login 
    set -l length $GENERATED_LENGTH
    set -l charset $CHARACTER_SET

    test -n "$login"; and set login "login: "$login
    test -z "$length"; and set length 30
    test -z "$charset"; and set charset "[:graph:]"

    head /dev/urandom | tr -dc $charset | read -n $length -l password
    
    if test -z "$login"
        printf "%s\n" $password | pass insert -m
    else
        printf "%s\n%s\n" $password $login | pass insert -m $argv
    end
end

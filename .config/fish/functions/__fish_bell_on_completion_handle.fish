function __fish_bell_on_completion_handle --on-event fish_postexec --description "Ring the terminal bell if a command that ran for more than 12 seconds has finished"
    if test $CMD_DURATION -ge 12000
        printf "\a"
    end
end

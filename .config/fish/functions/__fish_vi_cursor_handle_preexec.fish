function __fish_vi_cursor_handle_preexec --on-event fish_preexec
    if test $fish_bind_mode = "insert"
        echo -ne '\e[0 q'
    else
        echo -ne '\e[1 q'
    end
end

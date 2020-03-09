function fish_title
    # only set title for the terminal emulator
    if not string match -qe "$TERMINAL" "$TERM"
        return
    end

    # standard title
    echo $_ ' '
    pwd
end

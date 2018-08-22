function fish_title
    # emacs doesn't like this and for everything not xterm-* it doesn't matter
    if not string match -q 'xterm-*' $TERM
        return
    end

    # standard title
    echo $_ ' '
    pwd
end

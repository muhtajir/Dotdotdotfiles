if tty | grep -q pts ; then
    exec fish
fi

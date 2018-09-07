function __fish_man_page
    # hacky version of this command that allows nvim to work as a pager
    # Get all commandline tokens not starting with "-"
    set -l args (commandline -po | string match -rv '^-')

    # If commandline is empty, exit.
    if not set -q args[1]
        printf \a
        return
    end

    # If there are at least two tokens not starting with "-", the second one might be a subcommand.
    # Try "man first-second" and fall back to "man first" if that doesn't work out.
    set -l maincmd (basename $args[1])

    # Don't attempt to open a manpage unless we're sure it exists (this is the hacky part).
    set -l cmd

    if set -q args[2]
        man -w "$maincmd-$args[2]" >/dev/null 2>&1; and set cmd "$maincmd-$args[2]" 
        or man -w "$maincmd" >/dev/null 2>&1; and set cmd "$maincmd" 
    else
        man -w "$maincmd" >/dev/null 2>&1; and set cmd "$maincmd" 
    end

    if [ -n "$cmd" ]
        # finally we can do this without redirecting stderr
        man $cmd
    else
        printf \a
    end

    commandline -f repaint
    emit fish_prompt
end

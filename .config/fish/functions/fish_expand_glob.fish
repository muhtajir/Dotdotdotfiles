function fish_expand_glob
	# EVIL HACK: Escape globs manually, pass it to escape, and then "unescape" the globs again
    set -l tok (commandline -ct | string replace -a '*' '\*' | string replace -a '?' '\*' \
    | string escape | string replace -a '\\\\\*' '*' | string replace -a '\\\\\?' '?')
    set -l tokens
    eval set tokens "$tok"
    set -l tokens (string escape -- $tokens)
    if set -q tokens[1]
        commandline -tr ""
        commandline -i -- "$tokens"
    end
end

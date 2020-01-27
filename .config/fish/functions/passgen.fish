function passgen
    fish -c 'sleep .5; xdotool type (randstring)' 2>/dev/null &; pass insert -m $argv
end

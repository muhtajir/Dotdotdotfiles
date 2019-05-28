function mymail
    while pgrep mbsync >/dev/null
        echo -n 'Waiting for mbsync to finish.'
        sleep 2
        echo -ne '\r\033[2K'
    end

    set -l current_dir (pwd)
    set -l mutt_dir "$HOME/Downloads/Mutt"

    mkdir -p $mutt_dir

    mbsync -a
    cd $mutt_dir
    neomutt
    and mbsync -a >/dev/null &; disown

    cd $current_dir
end

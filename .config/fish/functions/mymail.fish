function mymail
    set -l current_dir (pwd)
    set -l mutt_dir "$HOME/Downloads/Mutt"

    mkdir -p $mutt_dir

    mbsync -a
    cd $mutt_dir
    neomutt
    and mbsync -a >/dev/null &; disown

    cd $current_dir
end

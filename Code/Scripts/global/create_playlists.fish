#!/bin/env fish

set new_since "2017-06-07"
set new_since (date -d $new_since +%s)

set playlist_folder "$HOME/.config/mpd/playlists"

function add_to_playlist -a playlist_name file_path
    set playlist_name $playlist_folder/$playlist_name.m3u
    for file in (find $file_path -name '*.flac' -or -name '*.ogg' -or -name '*.mp3')
        echo $file
        if test (stat -c '%X' $file) -gt $new_since
          and not string match -rq '^'$file'$' < $playlist_name
            echo $file >> $playlist_name
        end
    end
end

# build "recent singles"
set playlist_name 'Recent: Singles' 
add_to_playlist $playlist_name "$HOME/Musik/Singles"

# build "recent records"
for dir in (find $HOME/Musik/Alben/ -mindepth 1 -maxdepth 1 -type d)
    set playlist_name "Recent: "(basename $dir)
    add_to_playlist $playlist_name $dir
end

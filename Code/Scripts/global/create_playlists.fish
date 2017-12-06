#!/bin/env fish

set new_since "2017-06-07"
set new_since (date -d $new_since +%s)

set playlist_folder "$HOME/.config/mpd/playlists"

function get_files -a file_path
    find $file_path -name '*.flac' -or -name '*.ogg' -or -name '*.mp3'
end

function add_to_playlist -a playlist_name file_path shuffled
    set playlist_name $playlist_folder/$playlist_name.m3u
    set playlist_lines
    if test -e $playlist_name
        cat $playlist_name | while read line
            set playlist_lines $playlist_lines $line
        end
    end
    set playlist_array

    # fill array of files for the playlist
    for file in (get_files $file_path)
        if test (stat -c '%X' $file) -gt $new_since
          and not contains $file $playlist_lines
            echo $file
            set playlist_array $playlist_array $file
        end
    end

    # if array is empty, do nothing
    test (count $playlist_array) -lt 1; and return

    # write playlist to file
    string join \n $playlist_array >> $playlist_name

    # shuffle after writing if specified
    if test "$shuffled" = "true"
        set temp_file (mktemp)
        shuf $playlist_name > $temp_file
        mv $temp_file $playlist_name
    end

end

# build "recent singles"
set playlist_name 'Recent: Singles' 
add_to_playlist $playlist_name "$HOME/Musik/Singles" "true"

# build "recent records"
for dir in (find $HOME/Musik/Alben/ -mindepth 1 -maxdepth 1 -type d)
    set playlist_name "Recent: "(basename $dir)
    add_to_playlist $playlist_name $dir
end

#!/bin/env fish

set -q PASSWORD_STORE_DIR; or set -l PASSWORD_STORE_DIR "$HOME/.password-store"

set -l tmp_dir (mktemp -d "/dev/shm/XXXXXXXXXXXX")

function cp_to_cell -a file -S
    set -l tmp_file (mktemp -u "/dev/shm/XXXXXXXXXXXX")
    gpg -d -r 234E81E6 -o "$tmp_file" "$PASSWORD_STORE_DIR/$file" 2>/dev/null
    set -l new_file "$tmp_dir/$file"
    mkdir -p (dirname $new_file)
    gpg -e -r EFBC72E8 -o "$new_file" "$tmp_file" 2>/dev/null
    # adb push $new_file "/sdcard/.password-store/$sub_dir/"(basename $file)
    rm -rf "$tmp_file"
end

# prepar marked random passes
set -l portable_files
for line in (pass grep "portable: true")
    set line (string replace -ra '\e\[(K|[\d;]+m)' '' $line)
    if not string match -qe "portable: true" $line
        set -l file (string trim -c ':' $line)".gpg"
        set portable_files $portable_files $file
    end
end

for file in $portable_files
    cp_to_cell $file
end

# prepar non-random passes
for file in $PASSWORD_STORE_DIR/non-random/*.gpg
    cp_to_cell (string replace "$PASSWORD_STORE_DIR/" "" $file)
end

# push everything to phone
adb push $tmp_dir/* "/sdcard/.password-store/"

rm -rf "$tmp_dir"

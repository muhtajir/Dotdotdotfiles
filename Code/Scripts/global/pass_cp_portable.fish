#!/bin/env fish

set -q PASSWORD_STORE_DIR; or set -l PASSWORD_STORE_DIR "$HOME/.password-store"

function cp_to_cell -a file sub_dir
    set -l tmp_dir (mktemp -d "/dev/shm/XXXXXXXXXXXX")
    set -l tmp_file (mktemp -u "$tmp_dir/XXXXXXXXXXXX")
    gpg -d -r 234E81E6 -o $tmp_file $file
    set -l new_file "$tmp_dir/"(basename $file)
    gpg -e -r EFBC72E8 -o $new_file $tmp_file
    adb push $new_file "/sdcard/.password-store/$sub_dir/"(basename $file)
    rm -rf $tmp_dir
end

# copy marked random passes
set -l portable_files
for line in (pass grep "portable: true")
    set line (string replace -ra '\e\[(K|[\d;]+m)' '' $line)
    if not string match -qe "portable: true" $line
        set -l file $PASSWORD_STORE_DIR/(string trim -c ':' $line)".gpg"
        set portable_files $portable_files $file
    end
end

for file in $portable_files
    cp_to_cell $file "random"
end

# copy non-random passes
for file in $PASSWORD_STORE_DIR/non-random/*.gpg
    cp_to_cell $file "non-random"
end

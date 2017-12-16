#!/bin/env fish

set -q PASSWORD_STORE_DIR; or set -l PASSWORD_STORE_DIR "$HOME/.password-store"

set -l portable_files
for line in (pass grep "portable: true")
    set line (string replace -ra '\e\[(K|[\d;]+m)' '' $line)
    if not string match -qe "portable: true" $line
        set -l file $PASSWORD_STORE_DIR/(string trim -c ':' $line)".gpg"
        set portable_files $portable_files $file
    end
end

echo "Fetched portable passwords…"
sleep .5

for file in $portable_files
    set -l tmp_dir (mktemp -d "/dev/shm/XXXXXXXXXXXX")
    set -l tmp_file (mktemp -u "$tmp_dir/XXXXXXXXXXXX")
    gpg -d -r 234E81E6 -o $tmp_file $file
    set -l new_file "$tmp_dir/"(basename $file)
    gpg -e -r EFBC72E8 -o $new_file $tmp_file
    adb push $new_file "/sdcard/.password-store/random/"(basename $file)
    rm -rf $tmp_dir
end

echo "Pushing non-random passwords…"
sleep .5

adb push $PASSWORD_STORE_DIR/non-random/*.gpg /sdcard/.password-store/non-random/

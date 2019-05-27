#!/usr/bin/env fish

set tmpfile (mktemp)
cat - > $tmpfile
sed -n '1p' $tmpfile | string sub -s 7 | read dateline

string replace "$dateline" (date -d $dateline) < $tmpfile
rm $tmpfile

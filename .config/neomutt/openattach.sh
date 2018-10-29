#!/usr/bin/env fish

set -l tmpf (mktemp)
cp "$argv" "$tmpf"

set -l type (file -bi "$tmpf" | string match -r '^[^;]+')

if [ "$type" = "text/html" ]
    cp "$tmpf" "$tmpf".html
    set tmpf "$tmpf".html
end

open "$tmpf" >/dev/null &

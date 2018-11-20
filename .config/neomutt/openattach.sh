#!/usr/bin/env fish

set -l tmpf (mktemp)
cp "$argv" "$tmpf"

xdg-open "$tmpf" >/dev/null &

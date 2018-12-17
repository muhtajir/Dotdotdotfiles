function agtrack -d 'Grep all tracked files in current git repository' -a query
    for f in (git ls-tree master -r --name-only)
        if string match -qie "$query" < $f
            set_color -o; and echo "$f:"; and set_color normal
            grep -i "$query" "$f"
            echo ''
        end
    end
end

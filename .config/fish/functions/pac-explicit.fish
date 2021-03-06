function pac-explicit
    set -l pacs (pacman -Qetq)
    set -l explicits
    for pac in $pacs
        set -l group (pacman -Qi $pac | grep '^Gruppen' | string replace -r '^[^:]+:\s+(.+)$' '$1')
        string match -rq '^base' $group
        if test $status -ne 0
            set explicits $explicits $pac
        end
    end
    string join \n $explicits
end

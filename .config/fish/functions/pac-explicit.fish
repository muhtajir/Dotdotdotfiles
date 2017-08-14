# Defined in /tmp/fish.pGK51w/pac-explicit.fish @ line 2
function pac-explicit
	set -l pacs (pacman -Qeq)
    for pac in $pacs
        set -l group (pacman -Qi $pac | grep '^Gruppen' | string replace -r '^[^:]+:\s+(.+)$' '$1')
        string match -rq '^base' $group
        if test $status -ne 0
            echo $pac
        end
    end
end

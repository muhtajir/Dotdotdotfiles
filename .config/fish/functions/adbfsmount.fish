function adbfsmount --description "Provide a adbfs mountpoint as $adbfsmnt"
    if [ -d "$adbfsmnt" ]
        set_color -o; echo "Unmounting \$adbfsmnt ($adbfsmnt)..."; set_color normal
        fusermount -u "$adbfsmnt"
        and set -eU adbfsmnt
    else
        set -U adbfsmnt (mktemp -d --suffix "-adbbfs")
        set_color -o; echo "Mounting under $adbfsmnt (\$adbfsmnt)..."; set_color normal
        adbfs "$adbfsmnt"
        or set -eU adbfsmnt
    end
end

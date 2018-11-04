function aurmake -w cower -d 'Build specified AUR package'
    set -l pkgs (string match -r '(?<!\x2d)\b\S+' -- $argv)
    set -l args (string match -r '(?<!\S)\x2d\x2d\S+' -- $argv)

    for pkg in $pkgs
        set -l all_pkgs (auracle buildorder $pkg | string replace 'BUILD ' '')
        if [ $status -ne 0 ]
            return 1
        end

        if [ (count $all_pkgs) -gt 1 ]
            set_color -o
            echo "Building dependencies..."
            for dep in $all_pkgs[1..-2]
                __aurmake_single --asdeps $dep
                or return 1
            end
        end

        __aurmake_single $args $pkg
        or return 1

    end
end

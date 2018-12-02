function aurmake -w cower -d 'Build specified AUR package'
    set -l pkgs (string match -r '(?<!\x2d)\b\S+' -- $argv)
    set -l args (string match -r '(?<!\S)\x2d\x2d\S+' -- $argv)
    set -l all_deps

    true
    for pkg in $pkgs
        set -l all_pkgs (auracle buildorder $pkg | string match -r '(?<=^BUILD )\S+')

        # if the package itself is not included in the buildorder list include it here
        if [ "$all_pkgs[-1]" != $pkg ]
            set all_pkgs $all_pkgs $pkg
        end

        if [ (count $all_pkgs) -gt 1 ]
            set_color -o
            echo "Building dependencies..."
            for dep in $all_pkgs[1..-2]
                __aurmake_single --asdeps $dep
                or return 1
                set all_deps $all_deps $dep
            end
        end

        __aurmake_single $args $pkg
        or return 1

    end

    set_color -o
    set -l del_deps
    for dep in (pacman -Qdtq)
         contains $dep $all_deps; and set del_deps $del_deps $dep
    end
    if [ -n "$del_deps" ]
        echo "Removing dependencies installed from AUR..."
        sudo pacman -Rsc $del_deps
    end
end

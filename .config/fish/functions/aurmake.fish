function aurmake -w cower -d 'Build specified AUR package'
    set pkgs (string match -r '(?<!\x2d)\b\S+' -- $argv)
    set args (string match -r '(?<!\S)\x2d(\x2d)?\S+' -- $argv)

    # building packages
    for pkg in $pkgs
        # parsing `auracle buildorder`
        for dep in (auracle buildorder $pkg | string match 'AUR*')
            set dep (string split ' ' $dep)
            if not contains $dep[-1] $aur_deps
                set -a aur_deps $dep[-1]
            end
        end

        # building AUR dependencies
        for dep in $aur_deps
            set_color -o && echo "Building dependency $dep for package $pkg..." && set_color normal
            __aurmake_single --asdeps $dep
            or return 1
        end

        # building the target package
        __aurmake_single $args $pkg
        or return 1

    end

    # finding no longer needed build dependencies...
    set orphans (pacman -Qdtq)
    for dep in $aur_deps
        if contains $dep $orphans
            set -a del_deps $dep
        end
    end

    # ...and removing them
    if [ -n "$del_deps" ]
        set_color -o && echo "Removing build dependencies installed from AUR..." && set_color normal
        sudo pacman -Rsc $del_deps
    end
end

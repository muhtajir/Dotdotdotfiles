function __aurmake_single
    set -l args
    set -l pkg
    if [ (count $argv) -gt 1 ]
        set args $argv[1..-2]
        set pkg $argv[-1]
    else
        set pkg $argv
    end

    set -l pos (pwd)
    cd ~/Downloads/AUR/
    auracle download $pkg | string match -r '[^/]+$' | read -l pkg_folder
        or return 1

    cd $pkg_folder
    makepkg -sri $args $pkg
    
    cd $pos
end

function aurmake -w cower -d 'Build specified AUR package'
    set -l pos (pwd)
    cd ~/Downloads/AUR/

    auracle download $argv[-1] | string match -r '[^/]+$' | read -l pkg_folder
        or return

    cd $pkg_folder
    makepkg -sri $argv[1..-2]
    cd $pos
end

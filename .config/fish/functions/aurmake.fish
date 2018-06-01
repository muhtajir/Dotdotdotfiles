function aurmake
    if [ (count $argv) -gt 1 ]
        echo "Two many arguments."
        return
    end

    set -l pos (pwd)
    cd ~/Downloads/AUR/
    auracle download $argv | string match -r '[^/]+$' | read -l pkg_folder
        or return
    cd $pkg_folder
    makepkg -sri
    cd $pos
end

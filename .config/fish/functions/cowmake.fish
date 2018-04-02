function cowmake
    set -l pos (pwd)
    cower -d $argv; and cd $HOME/Downloads/AUR/$argv[-1]; and makepkg -sri
    cd $pos
end

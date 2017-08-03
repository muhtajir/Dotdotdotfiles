function cowmake
	cower -d $argv; and cd $HOME/Downloads/AUR/$argv[-1]; and makepkg -sri
end

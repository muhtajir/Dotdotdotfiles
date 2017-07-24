function cowmake
	cower -d $argv; and cd $HOME/Downloads/AUR/$argv; and makepkg -sri
end

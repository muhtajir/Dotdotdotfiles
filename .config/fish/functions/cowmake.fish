function cowmake
	cower -d $argv; and cd $aur/$argv; and makepkg -sri
end

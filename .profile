# include local bin in PATH
PATH=$PATH:$HOME/.local/share/bin
# merge configs to ensure recent changes are represented
merge_configs &

# export BASE16 colors from the termite config
export BASE00=$(grep -E '^color0\s+' < $HOME/.config/termite/config | sed -E 's/^color0\s+.+?#([a-fA-F0-9]+)/\1/') &
export BASE01=$(grep -E '^color18\s+' < $HOME/.config/termite/config | sed -E 's/^color18\s+.+?#([a-fA-F0-9]+)/\1/') &
export BASE02=$(grep -E '^color19\s+' < $HOME/.config/termite/config | sed -E 's/^color19\s+.+?#([a-fA-F0-9]+)/\1/') &
export BASE03=$(grep -E '^color8\s+' < $HOME/.config/termite/config | sed -E 's/^color8\s+.+?#([a-fA-F0-9]+)/\1/') &
export BASE04=$(grep -E '^color20\s+' < $HOME/.config/termite/config | sed -E 's/^color20\s+.+?#([a-fA-F0-9]+)/\1/') &
export BASE05=$(grep -E '^color7\s+' < $HOME/.config/termite/config | sed -E 's/^color7\s+.+?#([a-fA-F0-9]+)/\1/') &
export BASE06=$(grep -E '^color21\s+' < $HOME/.config/termite/config | sed -E 's/^color21\s+.+?#([a-fA-F0-9]+)/\1/') &
export BASE07=$(grep -E '^color15\s+' < $HOME/.config/termite/config | sed -E 's/^color15\s+.+?#([a-fA-F0-9]+)/\1/') &
export BASE08=$(grep -E '^color1\s+' < $HOME/.config/termite/config | sed -E 's/^color1\s+.+?#([a-fA-F0-9]+)/\1/') &
export BASE09=$(grep -E '^color16\s+' < $HOME/.config/termite/config | sed -E 's/^color16\s+.+?#([a-fA-F0-9]+)/\1/') &
export BASE0A=$(grep -E '^color3\s+' < $HOME/.config/termite/config | sed -E 's/^color3\s+.+?#([a-fA-F0-9]+)/\1/') &
export BASE0B=$(grep -E '^color2\s+' < $HOME/.config/termite/config | sed -E 's/^color2\s+.+?#([a-fA-F0-9]+)/\1/') &
export BASE0C=$(grep -E '^color6\s+' < $HOME/.config/termite/config | sed -E 's/^color6\s+.+?#([a-fA-F0-9]+)/\1/') &
export BASE0D=$(grep -E '^color4\s+' < $HOME/.config/termite/config | sed -E 's/^color4\s+.+?#([a-fA-F0-9]+)/\1/') &
export BASE0E=$(grep -E '^color5\s+' < $HOME/.config/termite/config | sed -E 's/^color5\s+.+?#([a-fA-F0-9]+)/\1/') &
export BASE0F=$(grep -E '^color17\s+' < $HOME/.config/termite/config | sed -E 's/^color17\s+.+?#([a-fA-F0-9]+)/\1/') &

wait

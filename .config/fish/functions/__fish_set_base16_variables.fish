function __fish_set_base16_variables
    set -l source_config "$HOME/.config/termite/config"

    set -Ux BASE00 (string replace -rf '^color0\s+.+?#([a-fA-F0-9]+)' '$1' < $source_config)
    set -Ux BASE01 (string replace -rf '^color18\s+.+?#([a-fA-F0-9]+)' '$1' < $source_config)
    set -Ux BASE02 (string replace -rf '^color19\s+.+?#([a-fA-F0-9]+)' '$1' < $source_config)
    set -Ux BASE03 (string replace -rf '^color8\s+.+?#([a-fA-F0-9]+)' '$1' < $source_config)
    set -Ux BASE04 (string replace -rf '^color20\s+.+?#([a-fA-F0-9]+)' '$1' < $source_config)
    set -Ux BASE05 (string replace -rf '^color7\s+.+?#([a-fA-F0-9]+)' '$1' < $source_config)
    set -Ux BASE06 (string replace -rf '^color21\s+.+?#([a-fA-F0-9]+)' '$1' < $source_config)
    set -Ux BASE07 (string replace -rf '^color15\s+.+?#([a-fA-F0-9]+)' '$1' < $source_config)
    set -Ux BASE08 (string replace -rf '^color1\s+.+?#([a-fA-F0-9]+)' '$1' < $source_config)
    set -Ux BASE09 (string replace -rf '^color16\s+.+?#([a-fA-F0-9]+)' '$1' < $source_config)
    set -Ux BASE0A (string replace -rf '^color3\s+.+?#([a-fA-F0-9]+)' '$1' < $source_config)
    set -Ux BASE0B (string replace -rf '^color2\s+.+?#([a-fA-F0-9]+)' '$1' < $source_config)
    set -Ux BASE0C (string replace -rf '^color6\s+.+?#([a-fA-F0-9]+)' '$1' < $source_config)
    set -Ux BASE0D (string replace -rf '^color4\s+.+?#([a-fA-F0-9]+)' '$1' < $source_config)
    set -Ux BASE0E (string replace -rf '^color5\s+.+?#([a-fA-F0-9]+)' '$1' < $source_config)
    set -Ux BASE0F (string replace -rf '^color17\s+.+?#([a-fA-F0-9]+)' '$1' < $source_config)
    
end

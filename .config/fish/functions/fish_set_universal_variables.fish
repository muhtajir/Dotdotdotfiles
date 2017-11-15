function fish_set_universal_variables
    set -U fish_greeting
    set -U fish_default_variables_set
    set -U fish_escape_delay_ms 10
    set -U fish_function_path_local $HOME/.config/fish/functions
    set -U FZF_TMUX_HEIGHT 60%
    set -U FZF_ALT_C_COMMAND "command find -P \$dir -mindepth 1 \( -wholename '*.config/*' -or \( -wholename '*.local/share/*' -and -not -wholename '*.local/share/Steam/*' \) \) -or -not -path '*/\.*' 2> /dev/null"
    set -U FZF_CTRL_T_COMMAND "command find -P \$dir -mindepth 1 \( -wholename '*.config/*' \) -or -not -path '*/\.*' 2> /dev/null"
    set -Ux MANPAGER "nvim -c 'set ft=man' -u /home/nicolai/.config/nvim/minimal_init.vim -"
end

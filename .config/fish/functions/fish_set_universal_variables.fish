function fish_set_universal_variables
    set -U fish_greeting
    set -U fish_default_variables_set
    set -U fish_escape_delay_ms 10
    set -U fish_function_path_local $HOME/.config/fish/functions
    set -U FZF_TMUX_HEIGHT 60%
    set -U FZF_ALT_C_COMMAND "find \( -wholename '*.config/*' -or \( -wholename '*.local/share/*' -and -not -wholename '*.local/share/Steam/*' \) \) -or -not -path '*/\.*' ^ /dev/null"
    set -Ux MANPAGER "nvim -c 'set ft=man' -u /home/nicolai/.config/nvim/minimal_init.vim -"
end

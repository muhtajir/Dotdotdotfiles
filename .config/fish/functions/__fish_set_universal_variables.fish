function __fish_set_universal_variables
    set -U fish_greeting
    set -U fish_default_variables_set
    set -U fish_escape_delay_ms 10
    set -U fish_function_path_local $HOME/.config/fish/functions

    # environment variables used by other programs
    # FZF
    set -U FZF_TMUX_HEIGHT 60%
    set -U FZF_ALT_C_COMMAND "command find -P \$dir -mindepth 1 \( -wholename '*.config/*' -or \( -wholename '*.local/share/*' -and -not -wholename '*.local/share/Steam/*' \) \) -or -not -path '*/\.*' 2> /dev/null"
    set -U FZF_CTRL_T_COMMAND "command find -P \$dir -mindepth 1 \( -wholename '*.config/*' -or \( -wholename '*.local/share/*' -and -not -wholename '*.local/share/Steam/*' \) \) -or -not -path '*/\.*' 2> /dev/null"
    # pass
    set -Ux PASSWORD_STORE_DIR $HOME/.local/share/.password-store

    # shell and pager color settings
    set -Ux LS_COLORS 'ow=34'
    set -Ux LESS_TERMCAP_md (printf "\e[01;36m")
    set -Ux LESS_TERMCAP_me (printf "\e[0m")
    set -Ux LESS_TERMCAP_se (printf "\e[0m")
    set -Ux LESS_TERMCAP_so (printf "\e[01;47;41m")
    set -Ux LESS_TERMCAP_ue (printf "\e[0m")
    set -Ux LESS_TERMCAP_us (printf "\e[01;35m")

end

function __fish_config_fzf
    set -Ux FZF_DEFAULT_OPTS --height 60% --border \
    --color=bg+:-1,bg:-1,spinner:{$__BASE0C_HASH},hl:{$__BASE0D_HASH},hl+:{$__BASE0D_HASH},fg:{$__BASE04_HASH},fg+:{$__BASE07_HASH} \
    --color=header:{$__BASE0D_HASH},info:{$__BASE0A_HASH},pointer:{$__BASE0C_HASH},marker:{$__BASE0C_HASH},prompt:{$__BASE09_HASH} \
    --bind=alt-j:down,alt-k:up --reverse

    set -Ux FZF_DEFAULT_COMMAND "command find -P \$dir -mindepth 1 \( -wholename '*.config/*' -or -wholename '*.emacs.d/*' -or \( -wholename '*.local/*' -and -not -wholename '*.local/share/Steam/*' \) \) -or -not -path '*/\.*' 2> /dev/null"

    set -U FZF_ALT_C_COMMAND $FZF_DEFAULT_COMMAND
    set -U FZF_CTRL_T_COMMAND $FZF_DEFAULT_COMMAND

    set -Ux FZF_OVERLAY_OPTS --no-border --margin 10%,8% --no-height --layout reverse-list
end

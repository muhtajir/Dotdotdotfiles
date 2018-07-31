# DO EDITS HERE: $HOME/.local/share/skel/fzf-shell.skel

function __fish_config_fzf
    set -Ux FZF_DEFAULT_OPTS "
    --height 60% --border
    --color=bg+:'#3a3a3a',bg:'#262626',spinner:'#85ad85',hl:'#83adad'
    --color=fg:'#949494',header:'#83adad',info:'#ffaf00',pointer:'#85ad85'
    --color=marker:'#85ad85',fg+:'#d5c4a1',prompt:'#ffaf00',hl+:'#83adad'
    --bind=alt-j:down,alt-k:up --reverse
    "

    set -Ux FZF_DEFAULT_COMMAND "command find -P \$dir -mindepth 1 \( -wholename '*.config/*' -or -wholename '*.emacs.d/*' -or \( -wholename '*.local/*' -and -not -wholename '*.local/share/Steam/*' \) \) -or -not -path '*/\.*' 2> /dev/null"
    set -U FZF_ALT_C_COMMAND $FZF_DEFAULT_COMMAND
    set -U FZF_CTRL_T_COMMAND $FZF_DEFAULT_COMMAND
end

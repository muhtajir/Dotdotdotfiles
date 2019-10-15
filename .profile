export FZF_DEFAULT_OPTS="
--height 60% --border
--color=bg+:-1,bg:-1,spinner:#${__BASE0C},hl:#${__BASE0D},hl+:#${__BASE0D},fg:#${__BASE04},fg+:#${__BASE07}
--color=header:#${__BASE0D},info:#${__BASE0A},pointer:#${__BASE0C},marker:#${__BASE0C},prompt:#${__BASE09}
--bind=alt-j:down,alt-k:up --reverse
"
export FZF_DEFAULT_COMMAND="command find -P \$dir -mindepth 1 \( -wholename '*.config/*' -or -wholename '*.emacs.d/*' -or \( -wholename '*.local/*' -and -not -wholename '*.local/share/Steam/*' \) \) -or -not -path '*/\.*' 2> /dev/null"
export FZF_OVERLAY_OPTS="--no-border --margin 10%,8% --no-height --layout reverse-list"
export FZF_ALT_C_COMMAND=$FZF_DEFAULT_COMMAND
export FZF_CTRL_T_COMMAND=$FZF_DEFAULT_COMMAND

ibus-daemon -xrd &
xset -b &
numlockx on &
(sleep 4 && xkbcomp ~/.config/xkb/custom.xkb "$DISPLAY") &

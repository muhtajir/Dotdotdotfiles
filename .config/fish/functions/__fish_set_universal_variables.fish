function __fish_set_universal_variables
    set -U fish_greeting
    set -U fish_default_variables_set
    set -U fish_escape_delay_ms 10
    set -U fish_function_path_local $HOME/.config/fish/functions

    # environment variables used by other programs
    # FZF
    set -U FZF_DEFAULT_OPTS "
      --height 60% --border
      --color=bg+:'#3a3a3a',bg:'#262626',spinner:'#85ad85',hl:'#83adad'
      --color=fg:'#949494',header:'#83adad',info:'#ffaf00',pointer:'#85ad85'
      --color=marker:'#85ad85',fg+:'#d5c4a1',prompt:'#ffaf00',hl+:'#83adad'
      --bind=alt-j:down,alt-k:up --reverse
    "
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

    # set base16 derived color variables
    for var in __BASE00 __BASE01 __BASE02 __BASE03 __BASE04 __BASE05 __BASE06 \
               __BASE07 __BASE08 __BASE09 __BASE0A __BASE0B __BASE0C __BASE0D \
               __BASE0E __BASE0F
        set -U {$var}_HASH '#'$$var

        set -U {$var}_R (printf '%d' 0x(string sub -l 2 $$var))
        set -U {$var}_G (printf '%d' 0x(string sub -s 3 -l 2 $$var))
        set -U {$var}_B (printf '%d' 0x(string sub -s 5 -l 2 $$var))
    end
end

# vim:ft=fish

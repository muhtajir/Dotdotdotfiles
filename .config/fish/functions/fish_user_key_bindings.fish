function fish_user_key_bindings
    fish_hybrid_key_bindings
    fzf_key_bindings
    
    # additional bindings that base fzf search in $MEDIA
    bind -M insert \eC 'commandline $MEDIA; and fzf-cd-widget'
    bind -M insert \et 'commandline -a $MEDIA; and fzf-file-widget'

    bind -M insert \ek up-or-search
    bind -M insert \ej down-or-search
    bind -M insert \el accept-autosuggestion

    # iso friendly Emacs keys
    bind -M insert \cf forward-word
    bind -M insert \cb backward-word

    # prepend/append commands by keypress
    bind -M insert \ep __fish_plz_bind
    bind -M insert \ea __fish_away_bind
    bind -M insert \cG __fish_lass_bind

    # better line editing
    bind -M insert \cV edit_command_buffer
    bind \cV edit_command_buffer

    # file selection with vifm
    bind -M insert \cf __fish_vifm_bind

    # update history but keep commandline
    bind -M insert \cH 'history merge'

    # toggle shadow mode
    bind -M insert \cs 'if set -q fish_private_mode; exec fish; else; exec fish --private; end'
end

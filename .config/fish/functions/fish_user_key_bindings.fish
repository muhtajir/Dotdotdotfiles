function fish_user_key_bindings
    if [ $fish_key_bindings != 'fish_vi_key_bindings' ]
        fish_vi_key_bindings
    end

    fish_default_key_bindings -M insert
    fzf_key_bindings
    # additional bindings that base fzf search in $MEDIA
    bind -M insert \eC 'commandline $MEDIA; and fzf-cd-widget'
    bind -M insert \et 'commandline -a $MEDIA; and fzf-file-widget'

    bind -M insert \ek up-or-search
    bind -M insert \ej down-or-search
    bind -M insert \el accept-autosuggestion

    # use escape to get to normal mode
    bind -M insert \e 'if commandline -P; commandline -f cancel; else; set fish_bind_mode default; commandline -f backward-char force-repaint; end'
    bind \e cancel
    bind -M visual -m default \e end-selection force-repaint
    bind -M replace-one -m default \e cancel force-repaint

    # iso friendly Emacs keys
    bind -M insert \cf forward-word
    bind -M insert \cb backward-word

    # prepend commands by keypress
    bind -M insert \ep __fish_plz_bind
    bind -M insert \ea __fish_away_bind

    # better line editing
    bind -M insert \cV edit_command_buffer
    bind \cV edit_command_buffer

    # file selection with vifm
    bind -M insert \cf __fish_vifm_bind

    # update history but keep commandline
    bind -M insert \cH 'history merge'

    # toggle shadow mode
    bind -M insert \cs 'shadow; commandline -f force-repaint force-repaint'
end

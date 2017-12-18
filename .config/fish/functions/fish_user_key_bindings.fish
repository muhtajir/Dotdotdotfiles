function fish_user_key_bindings
    if test $fish_key_bindings != 'fish_vi_key_bindings'
        fish_vi_key_bindings
    end

    fish_default_key_bindings -M insert
    fzf_key_bindings

	bind -M insert \ek up-or-search
    bind -M insert \ej down-or-search
    bind -M insert \el accept-autosuggestion

    # use escape to get to normal mode
    bind -M insert \e 'if commandline -P; commandline -f cancel; else; set fish_bind_mode default; commandline -f backward-char force-repaint; end'
    bind \e cancel
    bind -M visual -m default \e end-selection force-repaint
    bind -M replace-one -m default \e cancel force-repaint

    # prepend sudo to cmdline and run it
    bind -M insert \ep __fish_plz_bind

    # better line editing
    bind -M insert \cE edit_command_buffer
    bind \cE edit_command_buffer

    # file selection with vifm
    bind -M insert \cF __fish_vifm_bind

    # update history but keep commandline
    bind -M insert \cH 'history merge'
end

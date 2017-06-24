function fish_user_key_bindings
    if test $fish_key_bindings != 'fish_vi_key_bindings'
        fish_vi_key_bindings
    end

    fish_default_key_bindings -M insert
	bind -M insert \ek up-or-search
    bind -M insert \ej down-or-search
    bind -M insert \el accept-autosuggestion

    # use escape to get to normal mode
    bind -M insert -m default \e backward-char force-repaint
    bind -M visual -m default \e backward-char force-repaint
    bind -M replace-one -m default \e backward-char force-repaint

    # prepend sudo to cmdline and run it
    bind -M insert \ep fish_plz_bind

    # better line editing
    bind -M insert \cE edit_command_buffer
    bind \cE edit_command_buffer
end

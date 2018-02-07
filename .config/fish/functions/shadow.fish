function shadow -d "Stop logging commannd line entries to the history file"
    set -l shadow_file $HOME/.local/share/fish/shadow_history
    set -l link_target (readlink $HOME/.local/share/fish/shadow_history)

    if test -e $shadow_file; and test "$link_target" != '/dev/null'
        echo 'There is already a shadow history file.. Delete before proceeding.'
        return 1
    end

    test ! -e $shadow_file; and ln -s /dev/null $shadow_file
    true

    set -gx fish_history shadow
    set -gx __fish_shadow_mode 1
end

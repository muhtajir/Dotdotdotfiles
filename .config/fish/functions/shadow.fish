function shadow -d "Stop logging command line entries to the history file"
    history delete --exact --case-sensitive shadow
    set -l shadow_file $HOME/.local/share/fish/shadow_history
    set -l link_target (readlink $HOME/.local/share/fish/shadow_history)

    function handle_shadow_file -S
        if test ! -e $shadow_file
            return
        else if test "$link_target" = '/dev/null'
            rm $shadow_file
            return
        else
            echo 'There is a foreign history file with the name "shadow". Delete before proceeding.'
            return 1
        end
    end

    if test $__fish_shadow_mode -eq 0
        set -g __fish_backup_history_var $fish_history
        handle_shadow_file; or return 1
        ln -s /dev/null $shadow_file
        set -g fish_history shadow
        set -g __fish_shadow_mode 1
    else if test $__fish_shadow_mode -eq 1
        set -g fish_history $__fish_backup_history_var
        set -e __fish_backup_history_var
        set -g __fish_shadow_mode 0
        handle_shadow_file; or return 1
    end
end

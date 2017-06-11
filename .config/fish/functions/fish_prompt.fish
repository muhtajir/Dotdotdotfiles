function fish_prompt --description 'Write out the prompt'
    __fish_create_first_seg
    __fish_create_second_seg
end

function __fish_create_first_seg --description 'Put together first segment'
    # catch the exit status before anything else or it will be lost
    set -l return_code $status

    # now choose content for the first segment
    # highest ranking output is bind mode
    if test $fish_bind_mode != insert
        __fish_draw_first_seg $color16 " Φ "
    # second is last exit code
    else if test $return_code -gt 0
        __fish_draw_first_seg $color01 " $return_code "
    # least important is active jobs
    else if jobs > /dev/null ^ /dev/null
        set -l fish_last_job (jobs -lc | tail -n 1)
        __fish_draw_first_seg $color06 " $fish_last_job "
    # and after that it's just a percent sign
    else
        __fish_draw_first_seg $color04 ' %% '
    end
end

function __fish_create_second_seg --description 'Put together second segment'
    set -l shell_pwd (basename (prompt_pwd))
    # if we're in a git repository display git segment
    __fish_git_test
    switch $status
        case 0
            __fish_draw_git_seg "["(__fish_get_git_branch)"] $shell_pwd"
        case 1
            __fish_draw_second_seg $color02 $shell_pwd
        case 2
            if test (__fish_get_git_branch) = "master"
                __fish_draw_git_seg " $shell_pwd"
            else 
                __fish_draw_git_seg "["(__fish_get_git_branch)"] $shell_pwd"
            end
    end
end

function __fish_chcol -a fg bg --description 'Change foreground and optionally background color'
    if test -n "$bg"
        set_color -b $bg $fg
    else
        set_color $fg
    end
end

function __fish_draw_first_seg -a bg_color content
    __fish_chcol $bg_color; and printf '░▒▓'
    __fish_chcol $color18 $bg_color ; and printf $content
    set -g fish_color_prompt_first_seg_bg $bg_color
end

function __fish_draw_second_seg -a bg_color content
    __fish_chcol $fish_color_prompt_first_seg_bg $bg_color; and printf ''
    __fish_chcol $color18; and printf ' %s ' $content
    __fish_chcol $bg_color normal; and printf ' '
    __fish_chcol normal normal
end

function __fish_draw_git_seg -a content --description 'Put together a git segment that changes color if the repo is not in a clean state'
    set -l git_state (command git status --porcelain ^ /dev/null)
    if test -z "$git_state"
        __fish_draw_second_seg $color21 ""$content
    else
        __fish_draw_second_seg $color13 ""$content
    end
end

function __fish_git_test
    # no git prompt if in home repository unless we're in the base folder
    set -l git_toplevel (command git rev-parse --show-toplevel ^ /dev/null)
    if test "$git_toplevel" = $HOME
        if test (pwd) = $HOME
            return 2
        else
            return 1
        end
    else if command git rev-parse --is-inside-work-tree > /dev/null ^&1
        return 0
    else
        return 1
    end
end

function __fish_get_git_branch
    echo (command git rev-parse --abbrev-ref HEAD)
end


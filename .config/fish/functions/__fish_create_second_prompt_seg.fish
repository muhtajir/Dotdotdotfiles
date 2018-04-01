function __fish_create_second_prompt_seg --description 'Put together second prompt segment'
    set -l shell_pwd (basename (prompt_pwd))
    # if we're in a git repository display git prompt segment
    __fish_git_test
    switch $status
        case 0
            __fish_draw_git_prompt_seg "["(__fish_get_git_branch)"] $shell_pwd"
        case 1
            __fish_draw_second_prompt_seg $__BASE0B $shell_pwd
        case 2
            if test (__fish_get_git_branch) = "master"
                __fish_draw_git_prompt_seg " $shell_pwd"
            else 
                __fish_draw_git_prompt_seg "["(__fish_get_git_branch)"] $shell_pwd"
            end
    end
end

function __fish_draw_git_prompt_seg -a content --description 'Put together a git segment that changes color if the repo is not in a clean state'
    set -l git_state (command git status --porcelain ^ /dev/null)
    if test -z "$git_state"
        __fish_draw_second_prompt_seg brwhite ""$content
    else
        __fish_draw_second_prompt_seg brmagenta ""$content
    end
end

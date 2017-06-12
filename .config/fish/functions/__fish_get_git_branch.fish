function __fish_get_git_branch
    echo (command git rev-parse --abbrev-ref HEAD)
end

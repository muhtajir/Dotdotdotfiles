function __fish_get_git_branch
    set -l git_branch (command git rev-parse --abbrev-ref HEAD ^ /dev/null)
    test "$git_branch" = "HEAD"; and set git_branch "…"
    echo $git_branch
end

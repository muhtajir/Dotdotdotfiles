function tryout -d "Run command in temporary HOME folder"
    set -l cmd (string escape -- $argv)
    set -lx HOME (mktemp -d)
    eval "$cmd"
end

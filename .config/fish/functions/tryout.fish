function tryout -d "Run command in temporary HOME folder"
    set -lx HOME (mktemp -d)
    eval $argv
end

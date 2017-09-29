function py.test
    set -l curdir (pwd)

    if test -d tests
        true
    else if test -d ../tests
        cd ..
    else
        echo 'Directory "tests" not found.'
        return
    end

    set -lx PYTHONPATH (pwd)
    pytest $argv
    cd $curdir
end

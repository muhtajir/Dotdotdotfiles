function py.test
    set -l curdir (pwd)

    for t_folder in test tests
        if test -d $t_folder
            break
        else if test -d ../$t_folder
            cd ..
            break
        else
            false
        end
    end
    or begin
        echo "No testing folder found."
        return
    end

    

    set -lx PYTHONPATH (pwd)
    pytest $argv
    cd $curdir
end

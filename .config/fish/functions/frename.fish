function frename
    argparse 'n/dry-run' -- $argv

    # validate input
    if [ ! (count $argv) -ge 3 ]
        echo "Required format: (-n) PATTERN REPLACEMENT FILE(S)"
        return 2
    end

    set -l pattern $argv[1]
    set -l replacement $argv[2]
    set -l file_list $argv[3..-1]
    # validate file input
    for file in $file_list
        if [ ! \( -f $file -o -d $file \) ]
            echo "$file is not a file or directory."
            return 2
        end
    end

    # do the renaming
    for file in $file_list
        set -l new_name (string replace -r $argv[1] $argv[2] "$file")
        if set -ql _flag_n
            echo "$file --> $new_name"
        else
            mv "$file" "$new_name"
        end
    end
end

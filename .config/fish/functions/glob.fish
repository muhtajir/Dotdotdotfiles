function glob
    argparse -n 'glob' -x 'd,f' 'h/hidden' 'x/except=+' 'd/directory' 'f/file' -- $argv
        or return 1

    # build list of files
    set -l all_files *
    if [ "$_flag_h" ]
        set all_files $all_files .*
    end

    set -l out_files
    # loop through all_files and only put those we need into out_files
    for f in $all_files
        # skip for any exception defined on the command line
        set -l except ""
        for x in $_flag_except
            string match -q "$x" "$f"; and set except "$f"; and break
        end
        test "$except"; and continue

        if [ -n "$_flag_directory" -a ! -d "$f" ]
            continue
        else if [ -n "$_flag_file" -a ! -f "$f" ]
            continue
        else
            set out_files $out_files "$f"
        end
    end

    printf '%s\n' $out_files
end

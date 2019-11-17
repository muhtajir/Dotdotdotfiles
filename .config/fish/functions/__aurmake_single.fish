function __aurmake_single
    set args
    set pkg

    # the last argument is the package, everything else is assumed to be arguments
    if [ (count $argv) -gt 1 ]
        set args $argv[1..-2]
        set pkg $argv[-1]
    else
        set pkg $argv
    end

    set pos (pwd)
    mkdir -p "$AURMAKE_FOLDER"
    cd "$AURMAKE_FOLDER"
    set exit_status 0

    auracle download $pkg | string match -r '/\S+$' | read -l pkg_folder
    if [ $status = "0" ]
        cd $pkg_folder
        makepkg -sri $args $pkg; or set exit_status 1
    else
        set exit_status 1
    end

    cd $pos
    return $exit_status
end

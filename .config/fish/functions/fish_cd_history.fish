function fish_cd_history
    if test -n "$dirnext"
        set -l row (count $dirnext)
        for dir in $dirnext
            echo -e "+"$row"\t"$dir
            set row (math $row - 1)
        end
    end

    if test -n "$dirprev"
        set dircount (count $dirprev)
        set pos 1
        while test $dircount -gt 0
            echo -e "-"$pos"\t"$dirprev[$dircount]
            set dircount (math $dircount - 1)
            set pos (math $pos +1)
        end
    end
    return 0
end

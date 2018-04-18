function colortest
    printf 'BASE0%X ' (seq 0 15)
    for i in (seq 0 15)
        set fg __BASE0(printf '%X' $i)
        echo

        for k in (seq 0 15)
            set bg __BASE0(printf '%X' $k)
            set_color $$fg -b $$bg
            printf 'BASE0%X' $i
            set_color normal
            echo -n ' '
        end

        echo
    end
end

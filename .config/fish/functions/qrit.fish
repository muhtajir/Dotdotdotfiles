function qrit -d "Create a QR Code from the provided argument"
   [ -z "$argv" ]; and return 1

   set -l str_out $argv[1]
    for str in $argv[2..-1]
           set str_out $str_out" $str"
    end

   echo -n "$str_out" | qrencode --size 10 -o - | feh -. -
end

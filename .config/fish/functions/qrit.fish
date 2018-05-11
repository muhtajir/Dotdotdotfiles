function qrit -d "Create a QR Code from the provided argument"
   [ -z "$argv" ]; and return 1
    string join ' ' $argv | qrencode -o - | feh -. -
end

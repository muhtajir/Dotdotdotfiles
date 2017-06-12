function bootwin
    set -l win_entry (efibootmgr | grep -i windows | head -n 1 | string replace -r 'Boot(\d+).+' '$1') 
    command efibootmgr -n $win_entry
    command systemctl reboot
end


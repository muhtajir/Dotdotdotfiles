function dnscrypt --description 'Turn dnscrypt-proxy on or off'
    set -l resolv '/etc/resolv.conf'
    set -l nm_config '/etc/NetworkManager/NetworkManager.conf'
    set -l tmpf (mktemp)
    switch "$argv"
        case "on"
            echo "nameserver 127.0.0.1" > "$tmpf"
            sudo mv "$tmpf" $resolv

            if string match -rq '\bdns=' $nm_config
                string replace -r '\b(dns=)' '$1none' < $nm_config > "$tmpf"
                sudo mv "$tmpf" $nm_config
            else
                echo -e "[main]\ndns=none" > "$tmpf"
                sudo mv "$tmpf" $nm_config
            end

            for f in $resolv $nm_config
                sudo chown root:root $f
                sudo chmod 644 $f
            end

            if not systemctl is-active dnscrypt-proxy > /dev/null
                sudo systemctl start dnscrypt-proxy.socket
            end

        case "off"
            if string match -rq '\bdns=' $nm_config
                string replace -r '\b(dns=)' '$1default' < $nm_config > "$tmpf"
                sudo mv "$tmpf" $nm_config
            else
                echo "[main]"\n"dns=default" > "$tmpf"
                sudo mv "$tmpf" $nm_config
            end

            sudo chown root:root $nm_config
            sudo chmod 644 $nm_config

            if systemctl is-active dnscrypt-proxy > /dev/null
                sudo systemctl stop dnscrypt-proxy.socket
                sudo systemctl stop dnscrypt-proxy.service
            end

            sudo rm "$resolv"
            sudo systemctl restart NetworkManager

        case '*'
            echo "dnscrypt [on/off]"
    end
end

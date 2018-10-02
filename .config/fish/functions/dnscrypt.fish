function dnscrypt --description 'Turn dnscrypt-proxy on or off'
    set -l resolv '/etc/resolv.conf'
    set -l nm_config '/etc/NetworkManager/NetworkManager.conf'
    set -l tmpf (mktemp)
    switch "$argv"
        case "on"
            echo "127.0.0.1" > "$tmpf"
            sudo mv "$tmpf" "$nm_config"

            if string match -rq '\bdns=' "$nm_config"
                string replace -r '\b(dns=)' '$1none' < "$nm_config" > "$tmpf"
                sudo mv "$tmpf" "$nm_config"
            else
                echo "dns=none" > "$tmpf"
                sudo mv "$tmpf" "$nm_config"
            end

            if not systemctl is-active dnscrypt-proxy > /dev/null
                sudo systemctl start dnscrypt-proxy
            end

        case "off"
            if string match -rq '\bdns=' "$nm_config"
                string replace -r '\b(dns=)' '$default' < "$nm_config" > "$tmpf"
                sudo mv "$tmpf" "$nm_config"
            else
                echo "dns=default" > "$tmpf"
                sudo mv "$tmpf" "$nm_config"
            end

            if systemctl is-active dnscrypt-proxy > /dev/null
                sudo systemctl stop dnscrypt-proxy
            end

            sudo systemctl restart NetworkManager

        case '*'
            echo "dnscrypt [on/off]"
    end
end

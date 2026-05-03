set -l cachyos_config /usr/share/cachyos-fish-config/cachyos-config.fish
if test -e $cachyos_config
    source $cachyos_config
end

set SSH_AUTH_SOCK $XDG_RUNTIME_DIR/ssh-agent.socket
set SSL_CERT_DIR $HOME/.aspnet/dev-certs/trust:/etc/ssl/certs

# overwrite greeting
# potentially disabling fastfetch
#function fish_greeting
#    # smth smth
#end

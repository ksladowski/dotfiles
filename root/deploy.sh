#!/usr/bin/env bash

sudo bash -c '
    cp -r ./etc /

    systemctl daemon-reload
    systemctl enable --now mnt-media.mount

    if [ "$HOSTNAME" = rex ]; then
        systemctl enable --now wol@enp7s0
    fi
'

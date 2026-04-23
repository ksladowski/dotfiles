#!/usr/bin/env bash

sudo -s <<EOF
    stow --no-folding . -t /

    systemctl daemon-reload
    systemctl enable --now mnt-media.mount
EOF


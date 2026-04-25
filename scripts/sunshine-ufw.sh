#!/usr/bin/env bash

sudo -s <<EOF
    ufw allow 47984/tcp
    ufw allow 47989/tcp
    ufw allow 47990/tcp
    ufw allow 48010/tcp
    ufw allow 47998:48000/udp
EOF

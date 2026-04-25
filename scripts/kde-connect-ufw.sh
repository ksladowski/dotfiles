#!/usr/bin/env bash

sudo -s <<EOF
    ufw allow 1714:1764/tcp
    ufw allow 1714:1764/udp
EOF

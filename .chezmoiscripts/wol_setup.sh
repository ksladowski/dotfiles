#!/usr/bin/env bash

STATUS="$(systemctl is-active cronie.service)"

pacman -Qs cronie
CRONIE_INSTALLED=$?

sudo -s <<EOF
    
EOF

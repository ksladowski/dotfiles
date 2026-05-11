#!/usr/bin/env bash

sudo systemctl enable --now sshd

sudo ufw allow from 10.0.0.0/16 to any port 22 proto tcp

exit 0

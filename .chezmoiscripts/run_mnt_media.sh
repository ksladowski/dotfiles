#!/usr/bin/env bash

# Define the fstab entry
fstab_entry="truenas.lan:/mnt/tank0/media /home/kevin/media nfs _netdev,noauto,users,x-systemd.automount,x-systemd.mount-timeout=10,timeo=14,x-systemd.idle-timeout=1min 0 0"

# Define the target mount point
mount_point="/home/kevin/media"

# Create mount point if it doesn't exist
if [ ! -d "$mount_point" ]; then
    mkdir -p "$mount_point"
fi

# Check if the entry already exists in /etc/fstab
if ! grep -q "$fstab_entry" /etc/fstab; then
    # Append the entry to /etc/fstab
    echo "$fstab_entry" | sudo tee -a /etc/fstab > /dev/null
    echo "Entry added to /etc/fstab"
else
    echo "Entry already exists in /etc/fstab"
fi

# Reload systemd manager configuration
sudo systemctl daemon-reload

# Enable automount
sudo systemctl enable "media.mount"
sudo systemctl enable "media.automount"

exit 0

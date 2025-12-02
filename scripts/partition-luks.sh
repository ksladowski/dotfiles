#!/usr/bin/env bash

set -e

ENCRYPTFLAG=false

while getopts e flag
do
    case "${flag}" in
        e) ENCRYPTFLAG=true;;
    esac
done

shift $((OPTIND -1))

if [ "$#" -ne 1 ]; then
    echo "Usage: $0 /dev/sdX"
    exit 1
fi

DISK="$1"

echo "$DISK will be formatted. Are you sure? [y/N]"
read ANSWER
if [[ "$ANSWER" != "y" ]]; then
    echo "Aborted."
    exit 1
fi

parted --script "$DISK" mklabel gpt
parted --script "$DISK" mkpart primary 1MiB 1GiB
parted --script "$DISK" mkpart primary 1GiB 100%

BOOTPART="${DISK}1"
LVMPART="${DISK}2"

mkfs.fat -F32 -n EFI "${EFIPART}"

if [[ "$ENCRYPTFLAG" == true ]]; then
    cryptsetup luksFormat $"{LVMPART}" --label CRYPT
    cryptsetup open "${LVMPART}" lvm
fi

pvcreate /dev/mapper/lvm
vgcreate vg /dev/mapper/lvm
lvcreate -L 16G -n swap vg
lvcreate -l '100%FREE' -n root vg

mkswap -L swap /dev/vg/swap
mkfs.btrfs -L arch /dev/vg/root

mount /dev/vg/root /mnt
btrfs subvolume create /mnt/@
btrfs subvolume create /mnt/@home
btrfs subvolume create /mnt/@snapshots
btrfs subvolume create /mnt/@var_log
umount -mnt

mount -o compress=zstd,subvol=@ /dev/vg/root /mnt

mkdir -p /mnt/{home,snapshots,var/log,boot}

mount -o compress=zstd,noatime,subvol=@home /dev/vg/root /mnt/home
mount -o compress=zstd,noatime,subvol=@snapshots /dev/vg/root /mnt/snapshots
mount -o compress=zstd,noatime,subvol=@var_log /dev/vg/root /mnt/var/log
mount "$BOOTPART" /mnt/boot

swapon /dev/vg/swap

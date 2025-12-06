#!/usr/bin/env bash

INTREGEX='^[0-9]+$'

set -euo pipefail
trap 'echo "Error at line $LINENO"; exit 1' ERR

usage() {
    cat <<EOF
Usage: $0 [options]

Options:
  -e, --encrypted           Target is encrypted (used with --skip-disk)
  -d, --disk                Disk to format/install
  -s, --swap                Swap size (number only)
  -h, --help                Show this help and exit
EOF
}

ENCRYPT=false
DISK=""
SWAP="16"

while [[ $# -gt 0 ]]; do
    case "$1" in
        -e|--encrypted) ENCRYPT=true; shift ;;
        -d|--disk) DISK="$2"; shift 2 ;;
        -s|--swap) SWAP="$2"; shift 2 ;;
        -h|--help) usage; exit 0 ;;
        *) echo "Unknown option: $1"; usage; exit 1 ;;
    esac
done

echo "=========================================="
echo "Arch Linux Install Script"
echo "ksladowski"
echo "=========================================="

if [[ "$DISK" == "" ]]; then
    echo "Provide a disk (/dev/sdX)"
    read -r DISK
fi

echo "$DISK will be formatted. Are you sure? [y/N]"
read -r ANSWER
if [[ "$ANSWER" != "y" ]]; then
    echo "Aborted."
    exit 1
fi

parted --script "$DISK" mklabel gpt
parted --script "$DISK" mkpart primary 1MiB 1GiB
parted --script "$DISK" mkpart primary 1GiB 100%

EFIPART="${DISK}1"
ROOTPART="${DISK}2"

mkfs.vfat -F32 -n EFI "${EFIPART}"

if [[ "$ENCRYPT" == false ]]; then
    echo "Use LUKS? [y/N]"
    read -r ANSWER
    if [[ "$ANSWER" == "y" ]]; then
        ENCRYPT=true
    fi
fi

if [[ "$ENCRYPT" == true ]]; then
    cryptsetup luksFormat --type luks2 "${ROOTPART}"
    cryptsetup luksOpen "${ROOTPART}" linuxroot
fi

mkfs.btrfs -f -L linuxroot /dev/mapper/linuxroot

mount -o compress=zstd,noatime,subvol=/ /dev/mapper/linuxroot /mnt

btrfs subvolume create /mnt/home
btrfs subvolume create /mnt/snapshots
btrfs subvolume create /mnt/var
btrfs subvolume create /mnt/swap

mount -o compress=zstd,noatime,subvol=/home /dev/mapper/linuxroot /mnt/home
mount -o compress=zstd,noatime,subvol=/snapshots /dev/mapper/linuxroot /mnt/snapshots
mount -o compress=zstd,noatime,subvol=/var /dev/mapper/linuxroot /mnt/var
mount -o compress=zstd,noatime,subvol=/swap /dev/mapper/linuxroot /mnt/swap
mkdir /mnt/boot
mount -o fmask=0137,dmask=0027 "$BOOTPART" /mnt/boot

if [[ "$SWAP" == "" ]]; then
    echo "Enter desired swap size (g) default 16"
    read -r ANSWER
    if [[ $ANSWER =~ $INTREGEX ]]; then
        SWAP=$ANSWER
    fi
fi
echo "Using default ${SWAP}G swap size."
btrfs filesystem mkswapfile --size "${SWAP}g" --uuid clear /mnt/swap/swapfile
swapon /mnt/swap/swapfile

echo ""
echo "Sorting US mirrors by speed"
reflector --country US --age 24 --sort rate --save /etc/pacman.d/mirrorlist

echo ""
echo "Determining processor type"
CPUINFO="$(dmidecode -t 4 2>/dev/null || true)"
if [[ $CPUINFO == *AMD* ]]; then
    MICROCODE_PKG="amd-ucode"
elif [[ $CPUINFO == *Intel* ]]; then
    MICROCODE_PKG="intel-ucode"
else
    MICROCODE_PKG=""
fi

# Build package list and only include microcode if non-empty
PACKAGES=(base linux linux-firmware vim networkmanager sudo git cryptsetup btrfs-progs fwupd)
if [[ -n "${MICROCODE_PKG:-}" ]]; then
    PACKAGES+=("${MICROCODE_PKG}")
fi

pacstrap -K /mnt "${PACKAGES[@]}"

genfstab -U /mnt >> /mnt/etc/fstab

arch-chroot /mnt ln -sf /usr/share/zoneinfo/America/Chicago /etc/localtime
arch-chroot /mnt hwclock --systohc

echo "KEYMAP=us" > /mnt/etc/vconsole.conf

echo "en_US.UTF-8 UTF-8" >> /mnt/etc/locale.gen
arch-chroot /mnt locale-gen
echo "LANG=en_US.UTF-8" > /mnt/etc/locale.conf

echo ""
echo "Hostname:"
read -r HOSTNAME

echo "$HOSTNAME" > /mnt/etc/hostname
cat > /mnt/etc/hosts <<EOF
127.0.0.1   localhost
::1         localhost
127.0.1.1   $HOSTNAME.localdomain $HOSTNAME
EOF

echo "rw quiet bgrt_disable rootflags=subvol=/" >> /mnt/etc/kenel/cmdline

# Build mkinitcpio HOOKS line depending on encryption
# Include sd-encrypt only if encryption is in effect
if [[ "$ENCRYPTFLAG" == true ]]; then
    MKINIT_HOOKS='(systemd autodetect microcode modconf kms keyboard sd-vconsole sd-encrypt block filesystems fsck)'
else
    MKINIT_HOOKS='(systemd autodetect microcode modconf kms keyboard sd-vconsole block filesystems fsck)'
fi

cat > /mnt/etc/mkinitcpio.conf <<EOF
MODULES=()
BINARIES=()
FILES=()
HOOKS=${MKINIT_HOOKS}
EOF

cat > /mnt/etc/mkinitcpio.d/linux.preset <<EOF
ALL_config="/etc/mkinitcpio.conf"
ALL_kver="/boot/vmlinuz-linux"
PRESETS=('default')
default_uki="/boot/EFI/Linux/arch-linux.efi"
default_options="--splash /usr/share/systemd/bootctl/splash-arch.bmp"
EOF

arch-chroot /mnt mkinitcpio -P

echo ""
echo "Enter Username:"
read -r USERNAME

arch-chroot /mnt useradd -m -G wheel -s /bin/bash "$USERNAME"

echo ""
echo "Set password for $USERNAME:"
arch-chroot /mnt passwd "$USERNAME"

echo "%wheel ALL=(ALL:ALL) ALL" >> /mnt/etc/sudoers

arch-chroot /mnt bootctl install

printf "default arch-linux.efi\ntimeout 3\nconsole-mode auto" > /mnt/boot/loader/loader.conf

arch-chroot /mnt systemctl enable NetworkManager

sed -i 's/^[[:space:]]*#[[:space:]]*Color/Color/' /mnt/etc/pacman.conf

if ! grep -q '^ILoveCandy' /mnt/etc/pacman.conf; then
  sed -i '/^Color/ a ILoveCandy' /mnt/etc/pacman.conf
fi

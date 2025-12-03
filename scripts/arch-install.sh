#!/usr/bin/env bash

INTREGEX='^[0-9]+$'

set -euo pipefail
trap 'echo "Error at line $LINENO"; exit 1' ERR

echo "=========================================="
echo "Arch Linux Install Script"
echo "ksladowski"
echo "=========================================="

echo "Provide a disk (/dev/sdX)"
read -r DISK

echo "$DISK will be formatted. Are you sure? [y/N]"
read -r ANSWER
if [[ "$ANSWER" != "y" ]]; then
    echo "Aborted."
    exit 1
fi

parted --script "$DISK" mklabel gpt
parted --script "$DISK" mkpart primary 1MiB 1GiB
parted --script "$DISK" mkpart primary 1GiB 100%

BOOTPART="${DISK}1"
LVMPART="${DISK}2"

mkfs.fat -F32 -n EFI "${BOOTPART}"

ENCRYPTFLAG=false
echo "Use LUKS? [y/N]"
read -r ANSWER
if [[ "$ANSWER" == "y" ]]; then
    ENCRYPTFLAG=true
fi

# Volume group naming (dynamic)
echo "Enter volume group name (default: vg):"
read -r VG_NAME
if [[ -z "$VG_NAME" ]]; then
  VG_NAME="vg"
fi

# LUKS mapper name only if encryption requested
PHYSVOL="${LVMPART}"
if [[ "$ENCRYPTFLAG" == true ]]; then
    echo "Enter LUKS mapper name (default: ${VG_NAME}_crypt):"
    read -r MAPPER_NAME
    if [[ -z "$MAPPER_NAME" ]]; then
      MAPPER_NAME="${VG_NAME}_crypt"
    fi

    cryptsetup luksFormat "${LVMPART}" --label CRYPT
    cryptsetup open "${LVMPART}" "${MAPPER_NAME}"
    PHYSVOL="/dev/mapper/${MAPPER_NAME}"
fi

# create physical volume and volume group
pvcreate "$PHYSVOL"
vgcreate "$VG_NAME" "$PHYSVOL"

echo "Enter desired swap size in GiB. Default 16"
SWAPSIZE=16
read -r ANSWER
if [[ $ANSWER =~ $INTREGEX ]]; then
  SWAPSIZE=$ANSWER
else
  echo "Using default ${SWAPSIZE}G swap size."
fi

lvcreate -L "${SWAPSIZE}G" -n swap "$VG_NAME"
mkswap -L swap "/dev/${VG_NAME}/swap"

lvcreate -l '100%FREE' -n root "$VG_NAME"
mkfs.btrfs -L arch "/dev/${VG_NAME}/root"

mount "/dev/${VG_NAME}/root" /mnt
btrfs subvolume create /mnt/@
btrfs subvolume create /mnt/@home
btrfs subvolume create /mnt/@snapshots
btrfs subvolume create /mnt/@var_log
umount /mnt

mount -o compress=zstd,subvol=@ "/dev/${VG_NAME}/root" /mnt

mkdir -p /mnt/{home,snapshots,var/log,boot}

mount -o compress=zstd,noatime,subvol=@home "/dev/${VG_NAME}/root" /mnt/home
mount -o compress=zstd,noatime,subvol=@snapshots "/dev/${VG_NAME}/root" /mnt/snapshots
mount -o compress=zstd,noatime,subvol=@var_log "/dev/${VG_NAME}/root" /mnt/var/log
mount "$BOOTPART" /mnt/boot

swapon "/dev/${VG_NAME}/swap"

echo "Partitioning done"
echo "Pre-Installation Setup:"

echo "Setting timezone"
timedatectl set-ntp true
timedatectl set-timezone America/Chicago

echo ""
echo "Sorting US mirrors by speed"
reflector --verbose --country US --sort rate --save /etc/pacman.d/mirrorlist

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

pacstrap -K /mnt \
         base \
         linux \
         linux-firmware\
         vim\
         networkmanager\
         sudo\
         git\
         "${MICROCODE_PKG}"

genfstab -U /mnt >> /mnt/etc/fstab
# replace fmask and dmask for /boot
sed -i 's/fmask=0022,dmask=0022/fmask=0137,dmask=0027/' /mnt/etc/fstab

echo ""
echo "Hostname:"
read -r HOSTNAME

echo "$HOSTNAME" > /mnt/etc/hostname
cat > /mnt/etc/hosts <<EOF
127.0.0.1   localhost
::1         localhost
127.0.1.1   $HOSTNAME.localdomain $HOSTNAME
EOF

# Build mkinitcpio HOOKS line depending on encryption
if [[ "$ENCRYPTFLAG" == true ]]; then
  MKINIT_HOOKS='(systemd autodetect microcode modconf kms keyboard sd-vconsole block sd-encrypt sd-lvm2 filesystems fsck)'
else
  MKINIT_HOOKS='(systemd autodetect microcode modconf kms keyboard sd-vconsole block sd-lvm2 filesystems fsck)'
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

# Compose kernel cmdline. If encrypted, include rd.luks.name, otherwise only root=...
if [[ "$ENCRYPTFLAG" == true ]]; then
  ROOTUUID="$(blkid -s UUID -o value "$LVMPART")"
  echo "rd.luks.name=${ROOTUUID}=${MAPPER_NAME} root=/dev/${VG_NAME}/root rw quiet bgrt_disable" > /mnt/etc/kernel/cmdline
else
  echo "root=/dev/${VG_NAME}/root rw quiet bgrt_disable" > /mnt/etc/kernel/cmdline
fi

echo ""
echo "Creating chroot configuration script..."

cat > /mnt/root/configure.sh <<'EOF'

#!/bin/bash
set -e

echo "Configuring system inside chroot..."

ln -sf /usr/share/zoneinfo/America/Chicago /etc/localtime
hwclock --systohc

echo "en_US.UTF-8 UTF-8" >> /etc/locale.gen
locale-gen
echo "LANG=en_US.UTF-8" > /etc/locale.conf

echo ""
echo "Enter Username:"
read -r USERNAME

useradd -m -G wheel,audio,video,input -s /bin/bash "$USERNAME"

echo ""
echo "Set password for $USERNAME:"
passwd "$USERNAME"

echo "%wheel ALL=(ALL:ALL) ALL" >> /etc/sudoers
visudo -c

bootctl install

printf "default arch-linux.efi\ntimeout 3\neditor 0\nconsole-mode auto" > /boot/loader/loader.conf
printf "color\nILoveCandy" > /etc/pacman.conf

systemctl enable NetworkManager

echo ""
echo "=========================================="
echo "✓ Base configuration complete!"
echo "=========================================="

EOF

chmod +x /mnt/root/configure.sh

arch-chroot /mnt /root/configure.sh

rm /mnt/root/configure.sh

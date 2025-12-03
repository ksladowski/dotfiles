#!/usr/bin/env bash

INTREGEX='^[0-9]+$'

set -e

echo "=========================================="
echo "Arch Linux Install Script"
echo "ksladowski"
echo "=========================================="

echo "Provide a disk (/dev/sdX)"
read DISK

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

mkfs.fat -F32 -n EFI "${BOOTPART}"

ENCRYPTFLAG=false
echo "Use LUKS?"
read ANSWER
if [[ "$ANSWER" == "y" ]]; then
    ENCRYPTFLAG=true
fi

if [[ "$ENCRYPTFLAG" == true ]]; then
    cryptsetup luksFormat "${LVMPART}" --label CRYPT
    cryptsetup open "${LVMPART}" lvm
fi

# TODO this only works if luks is set up
pvcreate /dev/mapper/lvm
vgcreate vg /dev/mapper/lvm

SWAPSIZE=16
echo "Enter desired swap size in GiB. Default 16"
read ANSWER
if ! [[ $ANSWER =~ $INTREGEX ]] ; then
   echo "Using default 16G swap size." >&2;
fi

lvcreate -L "${SWAPSIZE}" -n swap vg
mkswap -L swap /dev/vg/swap

lvcreate -l '100%FREE' -n root vg
mkfs.btrfs -L arch /dev/vg/root

mount /dev/vg/root /mnt
btrfs subvolume create /mnt/@
btrfs subvolume create /mnt/@home
btrfs subvolume create /mnt/@snapshots
btrfs subvolume create /mnt/@var_log
umount /mnt

mount -o compress=zstd,subvol=@ /dev/vg/root /mnt

mkdir -p /mnt/{home,snapshots,var/log,boot}

mount -o compress=zstd,noatime,subvol=@home /dev/vg/root /mnt/home
mount -o compress=zstd,noatime,subvol=@snapshots /dev/vg/root /mnt/snapshots
mount -o compress=zstd,noatime,subvol=@var_log /dev/vg/root /mnt/var/log
mount "$BOOTPART" /mnt/boot

swapon /dev/vg/swap

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
CPUINFO=$(dmidecode -t 4)
if [[ $(CPUINFO) != *AMD* ]]; then
    echo "Found AMD Processor"
    MICROCODE_PKG="amd-ucode"
else if [[ $(CPUINFO) != *Intel* ]]; then
        echo "Found Intel Processor"
         MICROCODE_PKG="intel-ucode"
     fi
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
awk '/\/boot/ && /fmask=0022,dmask=0022/ {old=$0; sub(/fmask=0022,dmask=0022/,"fmask=0137,dmask=0027"); print "OLD: " old; print "NEW: " $0}' /mnt/etc/fstab

echo ""
echo "Hostname:"
read HOSTNAME

echo "$HOSTNAME" > /mnt/etc/hostname
cat > /mnt/etc/hosts <<EOF
127.0.0.1   localhost
::1         localhost
127.0.1.1   $HOSTNAME.localdomain $HOSTNAME
EOF

cat > /mnt/etc/mkinitcpio.conf <<EOF
MODULES=()
BINARIES=()
FILES=()
HOOKS=(systemd autodetect microcode modconf kms keyboard sd-vconsole block sd-encrypt lmv2 filesystems fsck)"
EOF

cat > /mnt/etc/mkinitcpio.d/linux.preset <<EOF
ALL_config="/etc/mkinitcpio.conf"
ALL_kver="/boot/vmlinuz-linux"
PRESETS=('default')
default_uki="/boot/EFI/Linux/arch-linux.efi"
default_options="--splash /usr/share/systemd/bootctl/splash-arch.bmp"
EOF

ROOTUUID="${blkid -s UUID -o value "$LVMPART"}"
echo "rd.luks.name=$ROOTUUID=lvm root=/dev/vg-root/root rw quiet bgrt_disable" > /etc/kernel/cmdline

echo ""
echo "Creating chroot configuration script..."

cat > /mnt/root/configure.sh <<EOF

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
read USERNAME

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

rm /root/configure.sh

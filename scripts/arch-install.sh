#!/usr/bin/env bash

INTREGEX='^[0-9]+$'

set -euo pipefail
trap 'echo "Error at line $LINENO"; exit 1' ERR

usage() {
  cat <<EOF
Usage: $0 [options]

Options:
  -s, --skip-disk           Skip disk partitioning/mount setup (assume /mnt already prepared)
  -e, --encrypted           Target is encrypted (used with --skip-disk)
      --vg-name NAME        Set volume group name (non-interactive)
      --mapper-name NAME    Set LUKS mapper name (non-interactive)
      --lvm-part PATH       LVM partition path (e.g. /dev/sda2), used to detect UUID if needed
      --root-uuid UUID      UUID of the LUKS partition (used to build rd.luks.name)
  -h, --help                Show this help and exit

Notes:
  When using --skip-disk the script will NOT create partitions, LVs, or mount filesystems.
  You must ensure the target root is mounted at /mnt and boot is mounted at /mnt/boot (if needed).
  If the target is encrypted and you skip disk setup, provide --root-uuid or --lvm-part so the script
  can compute rd.luks.name for the kernel cmdline.
EOF
}

# defaults
SKIP_DISK=false
ENCRYPTFLAG=false
VG_NAME=""
MAPPER_NAME=""
LVMPART=""
ROOTUUID_CLI=""

# parse args
while [[ $# -gt 0 ]]; do
  case "$1" in
    -s|--skip-disk) SKIP_DISK=true; shift ;;
    -e|--encrypted) ENCRYPTFLAG=true; shift ;;
    --vg-name) VG_NAME="$2"; shift 2 ;;
    --mapper-name) MAPPER_NAME="$2"; shift 2 ;;
    --lvm-part) LVMPART="$2"; shift 2 ;;
    --root-uuid) ROOTUUID_CLI="$2"; shift 2 ;;
    -h|--help) usage; exit 0 ;;
    *) echo "Unknown option: $1"; usage; exit 1 ;;
  esac
done

echo "=========================================="
echo "Arch Linux Install Script"
echo "ksladowski"
echo "=========================================="

if [[ "$SKIP_DISK" == false ]]; then
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
  if [[ -z "$VG_NAME" ]]; then
    echo "Enter volume group name (default: vg):"
    read -r VG_NAME
    if [[ -z "$VG_NAME" ]]; then
      VG_NAME="vg"
    fi
  fi

  PHYSVOL="${LVMPART}"
  if [[ "$ENCRYPTFLAG" == true ]]; then
      if [[ -z "$MAPPER_NAME" ]]; then
        echo "Enter LUKS mapper name (default: ${VG_NAME}_crypt):"
        read -r MAPPER_NAME
        if [[ -z "$MAPPER_NAME" ]]; then
          MAPPER_NAME="${VG_NAME}_crypt"
        fi
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

else
  # SKIP_DISK == true
  echo "Skipping disk setup. Verifying /mnt is prepared..."
  if ! mountpoint -q /mnt; then
    echo "Error: /mnt is not a mountpoint. When using --skip-disk you must mount the target root at /mnt."
    exit 1
  fi
  # If boot isn't mounted at /mnt/boot, warn (we won't attempt to mount)
  if [[ ! -d /mnt/boot ]] || ! mountpoint -q /mnt/boot; then
    echo "Warning: /mnt/boot is not a mountpoint. If boot is not mounted, later steps that write to /mnt/boot may fail."
  fi

  # set defaults for names if not provided
  if [[ -z "$VG_NAME" ]]; then
    VG_NAME="vg"
    echo "VG name not provided; defaulting to '$VG_NAME'."
  else
    echo "Using VG_NAME='$VG_NAME' (from CLI)."
  fi

  # If encrypted was passed on CLI, keep ENCRYPTFLAG true; otherwise default false
  if [[ "$ENCRYPTFLAG" == true ]]; then
    echo "Operating with encrypted target (--encrypted)."
    # If MAPPER_NAME not provided, leave blank but warn
    if [[ -z "$MAPPER_NAME" ]]; then
      echo "Warning: no --mapper-name given. If a mapper name is required later, supply it via --mapper-name."
    fi
    # Determine ROOTUUID if possible: prefer --root-uuid, else try blkid on --lvm-part if provided
    if [[ -z "$ROOTUUID_CLI" && -n "$LVMPART" ]]; then
      if blkid -s UUID -o value "$LVMPART" >/dev/null 2>&1; then
        ROOTUUID_CLI="$(blkid -s UUID -o value "$LVMPART")"
        echo "Detected LUKS UUID from $LVMPART: $ROOTUUID_CLI"
      fi
    fi
    if [[ -z "$ROOTUUID_CLI" ]]; then
      echo "Note: encrypted target requires the LUKS partition UUID to write rd.luks.name. Provide --root-uuid or --lvm-part."
      # we don't fail here immediately; we'll validate later before writing kernel cmdline
    fi
  else
    echo "Operating without disk setup and without encryption."
  fi
fi

echo "Partitioning done (or skipped)"
echo "Pre-Installation Setup:"

echo "Setting timezone"
timedatectl set-ntp true
timedatectl set-timezone America/Chicago

echo ""
echo "Sorting US mirrors by speed"
reflector --verbose -l 20 --sort rate --save /etc/pacman.d/mirrorlist

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
# Include sd-encrypt only if encryption is in effect
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

# rd.luks.name: if encrypted, we need a UUID. If we performed disk setup above, compute it.
if [[ "$ENCRYPTFLAG" == true ]]; then
  if [[ -z "$ROOTUUID_CLI" && -n "${LVMPART:-}" && "$SKIP_DISK" == false ]]; then
    ROOTUUID_CLI="$(blkid -s UUID -o value "$LVMPART")"
  fi

  if [[ -z "$ROOTUUID_CLI" ]]; then
    echo "Error: cannot determine LUKS partition UUID for rd.luks.name."
    echo "Provide --root-uuid or run without --skip-disk so the script can compute it."
    exit 1
  fi

  if [[ -z "$MAPPER_NAME" ]]; then
    echo "Warning: no mapper name provided; defaulting to '${VG_NAME}_crypt'"
    MAPPER_NAME="${VG_NAME}_crypt"
  fi

  echo "rd.luks.name=${ROOTUUID_CLI}=${MAPPER_NAME} root=/dev/${VG_NAME}/root rw quiet bgrt_disable" > /mnt/etc/kernel/cmdline
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

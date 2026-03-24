#!/usr/bin/env bash
set -e

[ $# -lt 1 ] && echo "usage: $0 /dev/sdX" && exit 1
[ ! -b "$1" ] && echo "$1 not a block device" && exit 1

d="$1"
h="${2:-genericlaptop}"

echo "$d -> $h"
echo "WARNING: destroys all data"
read -p "continue? " -r

ram=$(grep MemTotal /proc/meminfo | awk '{print $2}')
swap=$((ram / 2 / 1024))

wipefs -af "$d"
sgdisk -Z "$d"
sgdisk -n 1:0:+1G -t 1:ef00 "$d"
sgdisk -n 2:0:+${swap}M -t 2:8200 "$d"
sgdisk -n 3:0:0 -t 3:8304 "$d"
partprobe "$d"
sleep 1

[[ "$d" =~ nvme ]] && p="${d}p" || p="${d}"

mkfs.fat -F 32 "${p}1"
mkswap "${p}2"
mkfs.ext4 "${p}3"

mount "${p}3" /mnt
mkdir -p /mnt/boot
mount "${p}1" /mnt/boot
swapon "${p}2"

[ -d /etc/nixos/setup ] && src=/etc/nixos/setup || src="$(cd "$(dirname "$0")" && pwd)"

# Copy setup to a writable tmpdir so we can drop in the generated hardware config
# without needing to write to /mnt or a potentially read-only src
tmpdir=$(mktemp -d)
cp -r "$src/." "$tmpdir/"

# Write hardware config directly to the host dir (stdout, never touches /mnt)
nixos-generate-config --root /mnt --show-hardware-config > "$tmpdir/hosts/${h}/hardware-configuration.nix"

nixos-install --flake "$tmpdir#${h}" --impure

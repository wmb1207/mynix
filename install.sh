#!/usr/bin/env bash
set -e

[ $# -lt 1 ] && echo "usage: $0 /dev/sdX [host]" && exit 1
[ ! -b "$1" ] && echo "$1 not a block device" && exit 1

d="$1"
h="${2:-genericlaptop}"
default_user="${NIXOS_USER:-${USER:-wmb}}"
[ "$default_user" = root ] && default_user=wmb

echo "$d -> $h"
echo "WARNING: destroys all data on $d"
read -p "continue? " -r
read -r -p "username [$default_user]: " user
user="${user:-$default_user}"

case "$user" in
  root|"")
    echo "invalid username: $user" >&2
    exit 1
    ;;
esac

[ -d /etc/nixos/setup ] && src=/etc/nixos/setup || src="$(cd "$(dirname "$0")" && pwd)"

tmpdir=$(mktemp -d)
cp -r "$src/." "$tmpdir/"

# Partition, format and mount using disko
NIXOS_USER="$user" disko --mode destroy,format,mount --yes-wipe-all-disks --argstr diskDevice "$d" --flake "$tmpdir#${h}"

# Generate hardware config into the tmpdir so nixos-install can find it
nixos-generate-config --root /mnt --show-hardware-config > "$tmpdir/hosts/${h}/hardware-configuration.nix"

NIXOS_USER="$user" nixos-install --flake "$tmpdir#${h}" --impure --no-root-password

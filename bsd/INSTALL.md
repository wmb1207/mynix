# FreeBSD on Dell Latitude — Installation Guide

Target machine: Dell Latitude (Intel CPU/GPU, NVMe, Thunderbolt, Intel WiFi)
FreeBSD version: 14.x RELEASE

---

## 1. BIOS settings

Enter BIOS: **F2** at boot.

| Setting | Value | Why |
|---|---|---|
| Secure Boot | **Disabled** | FreeBSD bootloader is not signed |
| UEFI Boot Mode | **UEFI** (keep) | needed for GPT + ZFS |
| Intel Virtualization (VT-x) | **Enabled** | bhyve / Docker VM |
| VT-d | **Enabled** | device passthrough to VMs |
| Fast Boot | **Disabled** | can skip UEFI init, causes boot issues |
| SATA Operation | **AHCI** | NVMe is separate and fine as-is |
| TPM | either | FreeBSD doesn't use it |

Save and exit.

---

## 2. Download FreeBSD

Go to https://www.freebsd.org/where/ and download:

```
FreeBSD-14.x-RELEASE-amd64-memstick.img
```

The **memstick** image is the USB installer. The **disc1** ISO is for VMs.

Write to USB (from any Linux/macOS machine):

```sh
# find your USB device first
lsblk   # Linux
diskutil list   # macOS

# write (replace sdX / diskN with your device — double-check this)
dd if=FreeBSD-14.x-RELEASE-amd64-memstick.img of=/dev/sdX bs=1M status=progress
# macOS:
dd if=FreeBSD-14.x-RELEASE-amd64-memstick.img of=/dev/rdiskN bs=1m
```

---

## 3. Boot from USB

**F12** at boot → select USB device.

Select: **Boot Multi User** (default, just press Enter).

---

## 4. bsdinstall — step by step

### Welcome
→ **Install**

---

### Keymap
→ **United States of America Dvorak** (you use dvorak)

Test it works, then **Select**.

---

### Hostname
```
latitude
```

---

### Distribution Components

Check only these:

- [x] `base-dbg` — skip (saves space)
- [x] `kernel` — **yes**
- [x] `base` — **yes**
- [ ] `games` — skip
- [ ] `lib32` — skip (you're pure 64-bit)
- [ ] `ports` — skip (use pkg)
- [ ] `src` — skip unless you want to build kernel

Minimum: **kernel + base** is enough.

---

### Network

**Configure wired first** (plug in ethernet if available — much faster install).

Select your interface:
- Wired: `em0` or `igc0` (Intel Gigabit)
- Skip WiFi for now — configure after install

DHCP: **Yes**
IPv6: **No** (unless you need it)

---

### Mirror

Pick the closest. Default (ftp.freebsd.org) is fine.
If in Argentina: `ftp2.br.freebsd.org` (Brazil) is usually fast.

---

### Partitioning — use ZFS

→ **Auto (ZFS)**

This is the recommended choice for a modern laptop. ZFS gives you:
- Snapshots before risky changes (take one before `start.janet all`)
- Data checksums (catches bit rot on NVMe)
- Easy rollback if setup breaks something

Settings:

| Option | Value |
|---|---|
| Pool type | **stripe** (single NVMe disk) |
| Encrypt disks | your call — adds GELI passphrase at boot |
| Partition scheme | **GPT** |
| Swap size | **8G** (or match your RAM if ≤ 16GB) |
| Mirror swap | No |
| Encrypt swap | No |

Select your NVMe disk: typically `nvd0` or `nda0` (FreeBSD 14 uses `nda` for NVMe).

**Confirm** — this erases the disk.

Install proceeds (~5–10 min depending on mirror speed).

---

### Root password

Set a strong root password. You'll use this until `doas` is configured.

---

### Time zone

→ **America** → **Argentina** → **Buenos Aires**

---

### Date/Time

Accept current (NTP will sync it properly on first boot).

---

### System Services (enable these)

- [x] `sshd`
- [x] `moused`
- [x] `ntpdate`
- [x] `ntpd`
- [x] `dumpdev` — optional, useful for debugging crashes
- [ ] `powerd` — start.janet handles this
- [ ] `local_unbound` — skip

---

### Security hardening (recommended)

Enable all of these — they're sensible defaults:

- [x] `hide_uids`
- [x] `hide_gids`
- [x] `read_msgbuf` — restrict dmesg to root
- [x] `proc_debug`
- [x] `random_pid`
- [x] `clear_tmp`
- [x] `disable_syslogd`
- [x] `disable_sendmail`
- [x] `secure_console`

---

### Add user

You can add `wmb` here or let `start.janet` do it.
If you add it now:

- Username: `wmb`
- Shell: `sh` for now (loksh not installed yet)
- Groups: `wheel`
- Password: set it

---

### Final config

→ **Exit** → **Reboot**

Remove the USB when it says to.

---

## 5. First boot — before running bootstrap

Login as **root**.

### Check network

```sh
ifconfig
ping -c 2 freebsd.org
```

If no wired connection, configure WiFi:

```sh
# find your WiFi interface (Intel WiFi on Latitude = iwm0 or iwlwifi0)
ifconfig -a | grep -E "^[a-z]"

# scan
ifconfig iwm0 up
ifconfig iwm0 scan

# connect
wpa_passphrase "YourSSID" "YourPassword" >> /etc/wpa_supplicant.conf
wpa_supplicant -B -i iwm0 -c /etc/wpa_supplicant.conf
dhclient iwm0
```

To make WiFi persist across reboots, add to `/etc/rc.conf`:
```
wlans_iwm0="wlan0"
ifconfig_wlan0="WPA DHCP"
```

### Optional: take a ZFS snapshot before setup

```sh
zfs snapshot -r zroot@fresh-install
# if setup goes badly: zfs rollback -r zroot@fresh-install
```

---

## 6. Run bootstrap

```sh
fetch -o bootstrap.sh \
  https://raw.githubusercontent.com/YOUR_USER/YOUR_REPO/main/bsd/bootstrap.sh
sh bootstrap.sh
```

Or from USB:
```sh
mount -t msdosfs /dev/da0s1 /mnt   # adjust device
sh /mnt/bsd/bootstrap.sh /mnt/setup
```

Bootstrap installs `git` + `janet`, clones the repo, then runs:

```sh
janet start.janet
```

---

## 7. After setup

```sh
reboot
# login as wmb
startx          # fvwm3 starts

# docker VM (after Alpine installer completes):
doas janet /opt/setup/bsd/setup/docker.janet context
docker run --rm hello-world

# tailscale:
doas tailscale up
```

---

## Known gaps vs NixOS

| Feature | Status |
|---|---|
| Intel ipu6 camera | **not available** — Linux-only driver |
| Battery charge thresholds (40/80%) | **not available** — no TLP equivalent |
| `aarch64` emulation | **not available** — Linux binfmt |
| Docker native | replaced by **bhyve VM** |
| Wayland/fcitx5 | **untested** — X11 path works fine |
| Steam | via linux compat layer (`pkg install linux-steam-utils`) |

---

## Useful commands on FreeBSD

```sh
pkg search <name>         # find packages
pkg install <name>        # install
pkg upgrade               # upgrade all

sysctl hw.model           # CPU info
sysctl hw.acpi.battery    # battery info
acpiconf -i 0             # detailed battery
dmesg | grep drm          # GPU init
kldstat                   # loaded kernel modules

vm list                   # bhyve VMs
vm start docker           # start docker VM
vm console docker         # serial console into VM
```

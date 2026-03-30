#!/bin/sh
# bootstrap.sh — run this on a fresh FreeBSD install as root
#
# Gets you from bare metal to a running Janet orchestration.
# Assumes you have network access (wired or wifi already configured).
#
# Usage:
#   sh bootstrap.sh                        # prompts for repo URL
#   sh bootstrap.sh https://github.com/... # clone from URL
#   sh bootstrap.sh /media/usb/setup       # use local copy (USB drive)

set -e

REPO_DST="/opt/setup"
BOLD="\033[1m"
CYAN="\033[36m"
GREEN="\033[32m"
RED="\033[31m"
DIM="\033[2m"
RST="\033[0m"

step() { printf "\n${BOLD}${CYAN}── %s ──${RST}\n" "$1"; }
ok()   { printf "${GREEN}  ✓${RST} %s\n" "$1"; }
info() { printf "${DIM}  → %s${RST}\n" "$1"; }
fail() { printf "${RED}  ✗ %s${RST}\n" "$1"; exit 1; }
ask()  { printf "${BOLD}%s${RST} " "$1"; read -r REPLY; echo "$REPLY"; }

# ── sanity checks ────────────────────────────────────────────────────────────

[ "$(id -u)" = "0" ] || fail "run this as root (su or doas)"

uname -s | grep -q FreeBSD || fail "this script is for FreeBSD only"

step "FreeBSD setup bootstrap"
info "$(uname -v)"

# ── pkg ──────────────────────────────────────────────────────────────────────

step "initialising pkg"
if ! command -v pkg >/dev/null 2>&1; then
    info "bootstrapping pkg..."
    env ASSUME_ALWAYS_YES=yes pkg bootstrap
fi
pkg update -q
ok "pkg ready"

# ── core deps ────────────────────────────────────────────────────────────────

step "installing git + janet"
pkg install -y git janet
ok "git and janet installed"

# ── get the setup repo ───────────────────────────────────────────────────────

step "getting setup repo"

REPO_SRC="${1:-}"

if [ -z "$REPO_SRC" ]; then
    printf "\nWhere is the setup repo?\n"
    printf "  1) clone from git URL\n"
    printf "  2) local path (USB drive, NFS mount, etc.)\n"
    printf "\nChoice [1/2]: "
    read -r CHOICE
    case "$CHOICE" in
        1)
            printf "Git URL: "
            read -r REPO_SRC
            ;;
        2)
            printf "Local path: "
            read -r REPO_SRC
            ;;
        *)
            fail "unknown choice"
            ;;
    esac
fi

if echo "$REPO_SRC" | grep -qE '^https?://|^git@'; then
    info "cloning $REPO_SRC..."
    git clone "$REPO_SRC" "$REPO_DST"
    ok "cloned to $REPO_DST"
elif [ -d "$REPO_SRC" ]; then
    info "copying from $REPO_SRC..."
    cp -r "$REPO_SRC" "$REPO_DST"
    ok "copied to $REPO_DST"
else
    fail "cannot read repo from: $REPO_SRC"
fi

# ── hand off to janet ────────────────────────────────────────────────────────

BSD_DIR="$REPO_DST/bsd"

if [ ! -f "$BSD_DIR/start.janet" ]; then
    fail "expected $BSD_DIR/start.janet — is this the right repo?"
fi

step "handing off to start.janet"
info "cd $BSD_DIR && janet start.janet"
cd "$BSD_DIR"
exec janet start.janet

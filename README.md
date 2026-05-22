# nix-setup

NixOS configurations for the studiowmb infrastructure — workstations, servers, and self-hosted services.

## Hosts

### Servers (Proxmox VMs)

| Host | Role |
|---|---|
| `nginx` | Reverse proxy (nginx) + Cloudflare Tunnel (cloudflared) |
| `podman-server` | Container host — Forgejo, Excalidraw, Hoppscotch, registry, and more |
| `coredns` | Internal DNS (`wmb.arpa`) |
| `postgres` | PostgreSQL |
| `redis` | Redis |
| `mongo` | MongoDB |
| `tailscale` | Tailscale node |

### Workstations / Laptops

| Host | Description |
|---|---|
| `nixos` | Primary desktop |
| `rog` | ASUS ROG laptop |
| `latitude` | Dell Latitude laptop |
| `asahibook` | Apple Silicon MacBook (Asahi Linux) |
| `asahimini` | Apple Silicon Mac Mini (Asahi Linux) |

## Structure

```
hosts/          per-host configurations
modules/        shared NixOS modules
templates/      devshells and other templates
flakes/         standalone flakes
```

## Services

Public services are exposed via Cloudflare Tunnel at `studiowmb.com`:

| Service | URL |
|---|---|
| Forgejo | https://git.studiowmb.com |
| Excalidraw | https://draw.studiowmb.com |
| Hoppscotch | https://app.studiowmb.com |

Internal services are available on `wmb.arpa` within the local network.

## Configuration

Create a `config.yml` at the repo root before using any scripts (it is gitignored):

```yaml
user: your-username
ssh_key: ~/.ssh/id_rsa  # optional, used for remote deploys
```

`make.rb` reads this file to set the deploy user, SSH key, home-manager paths, and template substitutions. Falls back to `$USER` if the file is absent.

## Secrets

Secrets are kept as gitignored files in their respective host directories:

| File | Used by |
|---|---|
| `hosts/nginx/cloudflared-token.env` | Cloudflare Tunnel token |
| `hosts/podman-server/hoppscotch.env` | Hoppscotch environment |

Copy the `.example` files and fill in the values before building.

## Deploying

```bash
nixos-rebuild boot --flake .#<host> --target-host <user>@<ip> --sudo --impure
ssh <user>@<ip> sudo reboot
```

## Workstation ISO

`workstationiso` is the supported installer image for new x86_64 workstations.
Build it from the repo root:

```bash
ruby make.rb --iso
```

Boot the ISO and run:

```bash
install
```

For scripted installs:

```bash
install --host genericlaptop --disk /dev/nvme0n1 --user wmb --no-forgejo-token
```

The ISO installer intentionally installs only the generic workstation profiles
(`genericlaptop` and `genericdesktop`). Apply a final machine profile such as
`latitude` after first boot, once the generated hardware configuration is in
place.

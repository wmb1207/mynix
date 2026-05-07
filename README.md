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

## Secrets

Secrets are kept as gitignored files in their respective host directories:

| File | Used by |
|---|---|
| `hosts/nginx/cloudflared-token.env` | Cloudflare Tunnel token |
| `hosts/podman-server/hoppscotch.env` | Hoppscotch environment |

Copy the `.example` files and fill in the values before building.

## Deploying

```bash
nixos-rebuild boot --flake .#<host> --target-host wmb@<ip> --sudo --impure
ssh wmb@<ip> sudo reboot
```

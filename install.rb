#!/usr/bin/env ruby
# frozen_string_literal: true

require 'open3'
require 'json'
require 'fileutils'
require 'tmpdir'
require 'optparse'
require 'io/console'

# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------
def run!(cmd, env: {}, chdir: nil)
  puts "  -> #{cmd.join(' ')}"
  system(env, *cmd, chdir: chdir).tap do |ok|
    abort("!! Command failed: #{cmd.join(' ')}") unless ok
  end
end

def prompt(question, default: nil)
  label = default ? "#{question} [#{default}]" : question
  print "#{label}: "
  $stdout.flush
  input = gets.chomp
  input.empty? ? default : input
end

def prompt_secret(question)
  print "#{question}: "
  $stdout.flush
  input = STDIN.noecho(&:gets).to_s.chomp
  puts
  input
end

def pick(label, options)
  puts "\n#{label}:"
  options.each.with_index(1) { |o, i| puts "  #{i}) #{o}" }
  loop do
    print "choice: "
    $stdout.flush
    n = gets.chomp.to_i
    return options[n - 1] if n.between?(1, options.size)
    puts "  invalid — enter 1..#{options.size}"
  end
end

def confirm!(message)
  print "\n#{message} (yes/no): "
  $stdout.flush
  abort("Aborted.") unless gets.chomp.strip.downcase == "yes"
end

def command_available?(name)
  ENV.fetch("PATH", "").split(File::PATH_SEPARATOR).any? do |dir|
    File.executable?(File.join(dir, name))
  end
end

def preflight!(disk:, host:)
  abort("!! #{disk} is not a block device") unless File.blockdev?(disk)

  missing = %w[disko nixos-generate-config nixos-install].reject { |cmd| command_available?(cmd) }
  abort("!! Missing required installer tools: #{missing.join(', ')}") unless missing.empty?

  mounted, = Open3.capture2("findmnt", "--raw", "--noheadings", "--output", "SOURCE")
  if mounted.lines.any? { |line| line.start_with?(disk) }
    abort("!! #{disk} or one of its partitions is mounted")
  end

  unless Dir.exist?("/sys/firmware/efi")
    abort("!! UEFI firmware not detected; #{host} uses the GPT/EFI disk layout")
  end
end

# ---------------------------------------------------------------------------
# Disk discovery
# ---------------------------------------------------------------------------
def list_disks
  raw, = Open3.capture2("lsblk", "--json", "--output", "NAME,SIZE,TYPE,MODEL", "--nodeps")
  JSON.parse(raw)["blockdevices"]
    .select  { |d| d["type"] == "disk" }
    .map     { |d| { dev: "/dev/#{d["name"]}", size: d["size"], model: d["model"].to_s.strip } }
end

def format_disk(d)
  label = d[:model].empty? ? d[:dev] : "#{d[:dev]}  #{d[:model]}"
  "#{label}  (#{d[:size]})"
end

# ---------------------------------------------------------------------------
# Host discovery — ISO install targets only
# ---------------------------------------------------------------------------
INSTALLABLE_HOSTS = %w[
  genericlaptop
  genericdesktop
].freeze

# ---------------------------------------------------------------------------
# Source dir — works from ISO (/etc/nixos/setup) or repo clone
# ---------------------------------------------------------------------------
SRC = if Dir.exist?("/etc/nixos/setup")
        "/etc/nixos/setup"
      else
        File.expand_path("..", __FILE__)
      end

FORGEJO_HOST = "git.studiowmb.com"
MNT          = "/mnt"

# ---------------------------------------------------------------------------
# Main
# ---------------------------------------------------------------------------
options = {
  host: nil,
  disk: nil,
  user: ENV.fetch("USER", "wmb"),
  forgejo_token: nil,
  ask_token: true,
}

OptionParser.new do |o|
  o.banner = "Usage: install [--host HOST] [--disk /dev/XXX] [--user USER]"
  o.on("--host HOST", INSTALLABLE_HOSTS, "Install target: #{INSTALLABLE_HOSTS.join(', ')}") { |v| options[:host] = v }
  o.on("--disk DISK", "Target disk to erase") { |v| options[:disk] = v }
  o.on("--user USER", "Initial user name") { |v| options[:user] = v }
  o.on("--forgejo-token TOKEN", "Forgejo token for post-install ~/.netrc") { |v| options[:forgejo_token] = v }
  o.on("--no-forgejo-token", "Skip Forgejo token prompt") { options[:ask_token] = false }
end.parse!

puts "=== Workstation ISO Installer ==="

# ── Select disk ─────────────────────────────────────────────────────────────
disks = list_disks
abort("No disks found.") if disks.empty?
disk_labels = disks.map { |d| format_disk(d) }
disk        = options[:disk] || disks[disk_labels.index(pick("Select installation disk", disk_labels))][:dev]

# ── Select host ─────────────────────────────────────────────────────────────
host = options[:host] || pick("Select install profile", INSTALLABLE_HOSTS)

# ── User ────────────────────────────────────────────────────────────────────
user = options[:user]
user = prompt("Username", default: user) if user.nil? || user.empty?

# ── Forgejo token ───────────────────────────────────────────────────────────
token = options[:forgejo_token]
token = prompt_secret("Forgejo token (leave blank to skip)") if token.nil? && options[:ask_token]

preflight!(disk: disk, host: host)

confirm!("This will ERASE #{disk} and install NixOS (#{host}). Continue?")

# ── Prepare tmpdir ──────────────────────────────────────────────────────────
puts "\n[1/4] Preparing build directory..."
tmpdir = Dir.mktmpdir
FileUtils.cp_r("#{SRC}/.", tmpdir)
install_env = {
  "NIXOS_USER" => user,
  "PWD" => tmpdir,
}

# ── Disko ───────────────────────────────────────────────────────────────────
puts "\n[2/4] Partitioning and mounting #{disk} with disko..."
run!(["disko", "--mode", "disko", "--disk", "main", disk, "--flake", "#{tmpdir}##{host}"], env: install_env, chdir: tmpdir)

# ── Hardware config ─────────────────────────────────────────────────────────
puts "\n[3/4] Generating hardware configuration..."
hw_dir = "#{tmpdir}/hosts/#{host}"
FileUtils.mkdir_p(hw_dir)
hw_cfg, = Open3.capture2("nixos-generate-config", "--root", MNT, "--show-hardware-config")
File.write("#{hw_dir}/hardware-configuration.nix", hw_cfg)

# ── NixOS install ────────────────────────────────────────────────────────────
puts "\n[4/4] Installing NixOS..."
run!(["nixos-install", "--flake", "#{tmpdir}##{host}", "--impure", "--no-root-password"], env: install_env, chdir: tmpdir)

# ── Post-install bootstrap ───────────────────────────────────────────────────
puts "\nPost-install setup..."
user_home = "#{MNT}/home/#{user}"
FileUtils.mkdir_p(user_home)

if token && !token.empty?
  netrc = "#{user_home}/.netrc"
  File.write(netrc, "machine #{FORGEJO_HOST}\nlogin #{user}\npassword #{token}\n")
  FileUtils.chmod(0o600, netrc)
  puts "  .netrc written"
end

local_nix = "#{SRC}/modules/local.nix"
if File.exist?(local_nix)
  FileUtils.cp(local_nix, "#{user_home}/local.nix")
  puts "  local.nix copied"
end

# ── Done ─────────────────────────────────────────────────────────────────────
puts <<~DONE

  Done! Reboot and then:

    git clone https://#{FORGEJO_HOST}/studiowmb/setup ~/dev/nix/setup
    cd ~/dev/nix/setup

    # First-boot setup (token, local.nix, config.yml):
    ruby make.rb --init #{host}

    # Edit config.yml — fill in host IPs and verify token:
    $EDITOR config.yml

    # Apply:
    ruby make.rb --apply #{host}
DONE

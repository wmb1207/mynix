#!/usr/bin/env ruby
# frozen_string_literal: true

require 'open3'
require 'json'
require 'fileutils'
require 'tmpdir'
require 'optparse'
require 'io/console'
require 'yaml'

# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------
def run!(cmd, env: {}, chdir: nil)
  puts "  -> #{cmd.join(' ')}"
  opts = chdir ? { chdir: chdir } : {}
  system(env, *cmd, **opts).tap do |ok|
    abort("!! Command failed: #{cmd.join(' ')}") unless ok
  end
end

def capture!(cmd)
  out, err, status = Open3.capture3(*cmd)
  abort("!! Command failed: #{cmd.join(' ')}\n#{err}") unless status.success?
  out
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

def prompt_password(label)
  loop do
    password = prompt_secret(label)
    confirm = prompt_secret("Confirm #{label}")
    return password if !password.empty? && password == confirm

    puts "  passwords must be non-empty and match"
  end
end

def password_hash(password)
  out, err, status = Open3.capture3("mkpasswd", "--method=sha-512", "--stdin", stdin_data: password)
  abort("!! Failed to hash password:\n#{err}") unless status.success?
  out.strip
end

def default_install_user
  user = ENV.fetch("NIXOS_USER", "")
  user = ENV.fetch("USER", "") if user.empty? || user == "root"
  user.empty? || user == "root" ? "wmb" : user
end

def valid_install_user?(user)
  user.match?(/\A[a-z_][a-z0-9_-]*[$]?\z/) && user != "root"
end

def nix_string(value)
  value.to_s.inspect
end

def prompt_user(default:)
  loop do
    user = prompt("Username", default: default).to_s.strip
    return user if valid_install_user?(user)

    puts "  invalid username — use a normal Linux user name, not root"
  end
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

def lsblk_tree(device)
  raw = capture!(["lsblk", "--json", "--paths", "--output", "NAME,TYPE,MOUNTPOINTS", device])
  JSON.parse(raw).fetch("blockdevices", [])
end

def flatten_blockdevices(devices)
  Array(devices).compact.flat_map do |dev|
    [dev] + flatten_blockdevices(dev.fetch("children", []))
  end
end

def target_devices(disk)
  flatten_blockdevices(lsblk_tree(disk))
end

def target_device_names(disk)
  target_devices(disk).filter_map do |dev|
    name = dev["name"]
    name if name.is_a?(String) && !name.empty?
  end
end

def mounted_target_paths(disk)
  target_devices(disk).flat_map do |dev|
    name = dev["name"]
    next [] unless name.is_a?(String) && !name.empty?

    Array(dev["mountpoints"]).compact.map { |mountpoint| [name, mountpoint] }
  end
end

def active_swap_devices
  return [] unless File.readable?("/proc/swaps")

  File.readlines("/proc/swaps").drop(1).map { |line| line.split.first }.compact
end

def preflight!(disk:, host:)
  abort("!! #{disk} is not a block device") unless File.blockdev?(disk)

  missing = %w[disko nixos-generate-config nixos-install lsblk wipefs sgdisk partprobe udevadm swapoff mkpasswd].reject { |cmd| command_available?(cmd) }
  abort("!! Missing required installer tools: #{missing.join(', ')}") unless missing.empty?

  mounts = mounted_target_paths(disk)
  unless mounts.empty?
    details = mounts.map { |dev, mountpoint| "#{dev} mounted at #{mountpoint}" }.join(", ")
    abort("!! Refusing to install to #{disk}: #{details}")
  end

  unless Dir.exist?("/sys/firmware/efi")
    abort("!! UEFI firmware not detected; #{host} uses the GPT/EFI disk layout")
  end
end

def prepare_disk!(disk)
  devices = target_device_names(disk)
  children = devices.reject { |dev| dev == disk }.sort_by(&:length).reverse
  swaps = active_swap_devices & children

  puts "  target devices: #{([disk] + children).uniq.join(', ')}"
  swaps.each { |dev| run!(["swapoff", dev]) }
  (children + [disk]).each { |dev| run!(["wipefs", "--all", "--force", dev]) if File.blockdev?(dev) }
  run!(["sgdisk", "--zap-all", disk])
  run!(["partprobe", disk])
  run!(["udevadm", "settle"])
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

def write_config_yml(path, user:, token:)
  config = if File.exist?(path)
             YAML.safe_load(File.read(path)) || {}
           else
             {}
           end
  config["user"] = user
  config["forgejo_token"] = token.to_s
  File.write(path, config.to_yaml)
end

def write_install_secrets(tmpdir, user:, user_hash:, root_hash:)
  path = "#{tmpdir}/modules/install-secrets.nix"
  File.write(path, <<~NIX)
    { ... }:
    {
      users.users.root.hashedPassword = #{nix_string(root_hash)};
      users.users.${nix_string(user)}.hashedPassword = #{nix_string(user_hash)};
    }
  NIX
end

def prepare_repo_templates!(repo, user:, token:)
  config_path = "#{repo}/config.yml"
  FileUtils.cp("#{repo}/config.yml.example", config_path) unless File.exist?(config_path)
  write_config_yml(config_path, user: user, token: token)

  local_nix = "#{repo}/modules/local.nix"
  example_nix = "#{repo}/modules/local.nix.example"
  FileUtils.cp(example_nix, local_nix) if !File.exist?(local_nix) && File.exist?(example_nix)

  run!(["ruby", "make.rb", "--tmpl"], env: {
    "NIXOS_USER" => user,
    "USER" => user,
    "HOME" => "/home/#{user}",
  }, chdir: repo)
end

def install_target_repo!(tmpdir, host:, user:)
  user_home = "#{MNT}/home/#{user}"
  owner = if File.exist?(user_home)
            stat = File.stat(user_home)
            [stat.uid, stat.gid]
          else
            [1000, 100]
          end
  target_repo = "#{MNT}/home/#{user}/dev/nix/setup"
  FileUtils.rm_rf(target_repo)
  FileUtils.mkdir_p(File.dirname(target_repo))
  FileUtils.cp_r("#{tmpdir}/.", target_repo)
  FileUtils.rm_f("#{target_repo}/modules/install-secrets.nix")
  FileUtils.chown_R(*owner, user_home)
  puts "  setup repo installed at /home/#{user}/dev/nix/setup"
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
  user: nil,
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
user = if options[:user]
         options[:user].strip
       else
         prompt_user(default: default_install_user)
       end
abort("!! Invalid install user: #{user.inspect}; use a normal Linux user name, not root") unless valid_install_user?(user)

preflight!(disk: disk, host: host)

confirm!("This will ERASE #{disk} and install NixOS (#{host}). Continue?")

# ── Passwords ────────────────────────────────────────────────────────────────
root_password = prompt_password("Root password")
user_password = prompt_password("#{user} password")

# ── Forgejo token ───────────────────────────────────────────────────────────
token = options[:forgejo_token]
token = prompt_secret("Forgejo token (leave blank to skip)") if token.nil? && options[:ask_token]

# ── Prepare tmpdir ──────────────────────────────────────────────────────────
puts "\n[1/4] Preparing build directory..."
tmpdir = Dir.mktmpdir
FileUtils.cp_r("#{SRC}/.", tmpdir)
prepare_repo_templates!(tmpdir, user: user, token: token)
write_install_secrets(
  tmpdir,
  user: user,
  user_hash: password_hash(user_password),
  root_hash: password_hash(root_password),
)
install_env = {
  "NIXOS_USER" => user,
  "PWD" => tmpdir,
}

# ── Disko ───────────────────────────────────────────────────────────────────
puts "\n[2/4] Preparing, partitioning and mounting #{disk}..."
prepare_disk!(disk)
run!(
  [
    "disko",
    "--mode", "destroy,format,mount",
    "--yes-wipe-all-disks",
    "--argstr", "diskDevice", disk,
    "--flake", "#{tmpdir}##{host}",
  ],
  env: install_env,
  chdir: tmpdir,
)

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
install_target_repo!(tmpdir, host: host, user: user)

if token && !token.empty?
  netrc = "#{user_home}/.netrc"
  File.write(netrc, "machine #{FORGEJO_HOST}\nlogin #{user}\npassword #{token}\n")
  FileUtils.chmod(0o600, netrc)
  puts "  .netrc written"
end
stat = File.stat(user_home)
FileUtils.chown_R(stat.uid, stat.gid, user_home)

# ── Done ─────────────────────────────────────────────────────────────────────
puts <<~DONE

  Done! Reboot and then:

    log in as #{user}
DONE

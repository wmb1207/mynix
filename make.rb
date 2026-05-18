#!/usr/bin/env ruby
# frozen_string_literal: true

require 'open3'
require 'optparse'
require 'pathname'
require 'yaml'

# ---------------------------------------------------------------------------
# Config
# ---------------------------------------------------------------------------
_cfg     = File.exist?("config.yml") ? (YAML.safe_load(File.read("config.yml")) || {}) : {}
USER     = _cfg.fetch("user",    ENV.fetch("USER", "wmb")).freeze
SSH_KEY  = _cfg["ssh_key"]&.then { |k| File.expand_path(k) }.freeze

# ---------------------------------------------------------------------------
# Color palette — gloomy creamsody
# ---------------------------------------------------------------------------
BLACK     = "#1c1a18"
BG_ALT    = "#252320"
SELECTION = "#302d29"
DARK_GRAY = "#6a6858"
WHITE     = "#b5b2a0"
RED       = "#884545"
OLIVE     = "#8a7040"
GREEN     = "#657050"
BLUE      = "#4a6a78"
MAUVE     = "#785a5a"
CREAM     = "#9a9888"

FONT                = "DejaVu Sans Mono"
TEMPLATES_DIR       = "templates"
ASSETS_DIR          = "assets"
TRANSPARENCY        = "100"
THEME               = "creamsody-darker"
LIGHT_THEME         = "ef-day"
GHOSTTY_THEME       = "Wez"
GHOSTTY_THEME_LIGHT = "GruvboxLight"

REMOTE_HOSTS = {
  "nginx"         => "192.168.88.26",
  "podman-server" => "192.168.88.38",
  "coredns"       => "192.168.88.27",
  "postgres"      => "192.168.88.10",
  "vm"            => "192.168.88.37",
  "redis"         => "192.168.88.11",
}.freeze

EMACS_PATHS = [
  Pathname("#{Dir.home}/.emacs.d/init.el"),
  Pathname("#{Dir.home}/.emacs.d/lisp/packages.el"),
].freeze

STAGED_ASSETS = %w[
  assets/init.el
  assets/bspwmrc
  assets/sxhkdrc
  assets/dunstrc
  assets/polybar.ini
  assets/fvwm3.conf
].freeze

# ---------------------------------------------------------------------------
# Data records
# ---------------------------------------------------------------------------
TemplateField = Struct.new(:key, :value, keyword_init: true)
Template      = Struct.new(:name, :output, :content, :fields, keyword_init: true)

# ---------------------------------------------------------------------------
# Shell helpers
# ---------------------------------------------------------------------------
def run_cmd(cmd, env: {})
  puts "Executing: #{cmd.join(' ')}"
  out, status = Open3.capture2e(env, *cmd)
  { exit: status.exitstatus, out: out }
end

def run_cmd!(cmd, env: {})
  run_cmd(cmd, env: env).then do |r|
    r[:exit].zero? ? r : abort("!! Command failed (exit #{r[:exit]}):\n#{r[:out]}")
  end
end

# ---------------------------------------------------------------------------
# Template helpers
# ---------------------------------------------------------------------------
def tmpl(name, out_file, tmpl_file, fields_hash)
  Template.new(
    name:    name,
    output:  "#{ASSETS_DIR}/#{out_file}",
    content: File.read("#{TEMPLATES_DIR}/#{tmpl_file}"),
    fields:  fields_hash.map { |k, v| TemplateField.new(key: k, value: v) }
  ).freeze
end

def apply_tmpl(tmpl)
  rendered = tmpl.fields.reduce(tmpl.content) { |acc, f| acc.gsub(f.key, f.value) }
  File.write(tmpl.output, rendered)
  puts "Applied template: #{tmpl.name} -> #{tmpl.output}"
end

def apply_tmpls!(tmpls)
  puts "Applying templates..."
  tmpls.each { |t| apply_tmpl(t) }
end

# ---------------------------------------------------------------------------
# Template definitions
# ---------------------------------------------------------------------------
def polybar_tmpl
  tmpl("polybar", "polybar.ini", "polybar.ini.tmpl", {
    "{{background}}"         => BLACK,
    "{{foreground}}"         => WHITE,
    "{{focused-background}}" => BLUE,
    "{{focused-foreground}}" => BLACK,
    "{{font}}"               => FONT,
  })
end

def bspwmrc_tmpl
  tmpl("bspwmrc", "bspwmrc", "bspwmrc.tmpl", {
    "{{active}}"        => "\\#{BLUE}",
    "{{normal-border}}" => "\\#{SELECTION}",
  })
end

def sxhkdrc_tmpl
  tmpl("sxhkdrc", "sxhkdrc", "sxhkdrc.tmpl", {
    "{{background}}"         => BLACK,
    "{{font}}"               => FONT,
    "{{selected-foreground}}" => BLACK,
    "{{foreground}}"         => WHITE,
    "{{active}}"             => BLUE,
  })
end

def ghostty_tmpl(light: false)
  tmpl("ghostty", "ghostty", "ghostty.tmpl", {
    "{{background}}"   => light ? WHITE : BLACK,
    "{{transparency}}" => TRANSPARENCY,
    "{{theme}}"        => light ? GHOSTTY_THEME_LIGHT : GHOSTTY_THEME,
    "{{font}}"         => FONT,
    "{{user}}"         => USER,
  })
end

def fvwm3_tmpl
  tmpl("fvwm3", "fvwm3.conf", "fvwm3.conf.tmpl", {
    "{{background}}"  => BLACK,
    "{{foreground}}"  => WHITE,
    "{{bg-alt}}"      => BG_ALT,
    "{{selection}}"   => SELECTION,
    "{{active}}"      => BLUE,
    "{{active-alt}}"  => CREAM,
    "{{comments}}"    => DARK_GRAY,
    "{{green}}"       => GREEN,
    "{{olive}}"       => OLIVE,
    "{{theme}}"       => THEME,
    "{{font}}"        => FONT,
    "{{user}}"        => USER,
  })
end

def dunstrc_tmpl
  tmpl("dunstrc", "dunstrc", "dunstrc.tmpl", {
    "{{black}}"        => BG_ALT,
    "{{frame}}"        => BLUE,
    "{{green}}"        => GREEN,
    "{{red}}"          => RED,
    "{{white}}"        => WHITE,
    "{{transparency}}" => TRANSPARENCY,
    "{{font}}"         => FONT,
  })
end

def emacs_tmpl(light: false)
  tmpl("emacs", "init.el", "init.el.tmpl", {
    "{{transparency}}" => TRANSPARENCY,
    "{{theme}}"        => light ? LIGHT_THEME : THEME,
    "{{font}}"         => FONT,
    "{{background}}"   => light ? WHITE : BLACK,
  })
end

def all_templates(light: false)
  [polybar_tmpl, bspwmrc_tmpl, sxhkdrc_tmpl, emacs_tmpl(light: light), dunstrc_tmpl, fvwm3_tmpl]
end

# ---------------------------------------------------------------------------
# Operations
# ---------------------------------------------------------------------------
def ensure_sudo!
  result = run_cmd(%w[doas true])
  if result[:exit].zero?
    puts "Doas authenticated."
  else
    abort("!! Failed to authenticate doas.")
  end
end

def apply_flake(host)
  run_cmd!(%W[doas git config --global --add safe.directory #{Dir.pwd}])
  result = run_cmd!(%W[doas nixos-rebuild switch --flake .##{host} --upgrade --impure])
  puts "Flake applied on #{host}\n#{result[:out]}"
end

def apply_boot_flake(host)d
  run_cmd!(%W[doas nixos-rebuild boot --flake .##{host} --upgrade --impure])
  puts "Flake applied on boot on #{host}\n#{result[:out]}"
end

def build_iso(host)
  result = run_cmd!(%W[nix build .#nixosConfigurations.#{host}.config.system.build.isoImage --impure])
  puts "ISO build done for #{host}\n#{result[:out]}"
end

def build_qcow2(host)
  result = run_cmd!(%W[nix build .#nixosConfigurations.#{host}.config.system.build.qcow2 --impure])
  puts "qcow2 build done for #{host}\n#{result[:out]}"
end

def build_pi(host)
  result = run_cmd!(%W[nix build .#nixosConfigurations.#{host}.config.system.build.sdImage --impure])
  puts "Pi image build done for #{host}\n#{result[:out]}"
end

def deploy(host)
  ip = REMOTE_HOSTS.fetch(host) do
    abort("!! Unknown host: #{host} — known: #{REMOTE_HOSTS.keys.join(', ')}")
  end
  env = SSH_KEY ? { "NIX_SSHOPTS" => "-i #{SSH_KEY}" } : {}
  result = run_cmd!(%W[
    nixos-rebuild switch
    --flake .##{host}
    --target-host #{USER}@#{ip}
    --sudo
    --impure
    --option require-sigs false
  ].flatten, env: env)
  puts "Deployed #{host} to #{ip}\n#{result[:out]}"
end

def clear_garbage
  run_cmd!(%w[doas nix-collect-garbage -d])
end

def remove_file(path)
  if path.exist?
    puts "Removing #{path}"
    run_cmd!(%W[doas rm -f #{path}])
    puts "Removed #{path}"
  else
    puts "#{path} not found, skipping."
  end
end

def remove_init_el
  EMACS_PATHS.each { |p| remove_file(p) }
end

def stage_assets
  run_cmd!(["git", "add", *STAGED_ASSETS])
  puts "Assets staged."
end

def activate_home_manager
  activate = "#{ENV.fetch('HOME')}/.local/state/home-manager/gcroots/current-home/activate"
  result = run_cmd!([activate])
  puts "Home-manager activation done.\n#{result[:out]}"
end

def run_all(host, light: false)
  apply_tmpls!(all_templates(light: light))
  stage_assets
  ensure_sudo!
  remove_init_el
  yield host
end

# ---------------------------------------------------------------------------
# CLI
# ---------------------------------------------------------------------------
def parse_opts(argv)
  opts = {}
  parser = OptionParser.new do |o|
    o.on("-c", "--clear",  "Clear all previous nix store instances")    { opts[:clear]  = true }
    o.on("-a", "--apply",  "Apply flake and templates")                 { opts[:apply]  = true }
    o.on("-t", "--tmpl",   "Build templates only")                      { opts[:tmpl]   = true }
    o.on("-p", "--pi",     "Build the Pi image")                        { opts[:pi]     = true }
    o.on("-i", "--iso",    "Build the ISO")                             { opts[:iso]    = true }
    o.on("-q", "--qcow2",  "Build a qcow2 disk image")                  { opts[:qcow2]  = true }
    o.on("-d", "--deploy", "Deploy to remote host via nixos-rebuild")   { opts[:deploy] = true }
    o.on("-l", "--light",  "Use light theme variant")                   { opts[:light]  = true }
    o.on("-b", "--boot",   "Apply flake and templates whith Boot flag") { opts[:boot]  = true }
  end
  rest = parser.parse(argv)
  [opts, rest]
end

def main(argv)
  opts, rest = parse_opts(argv)
  host  = rest.first
  light = opts[:light] || false

  case
  when opts[:iso]    then build_iso(host)
  when opts[:pi]     then build_pi(host)
  when opts[:qcow2]  then build_qcow2(host)
  when opts[:tmpl]   then apply_tmpls!(all_templates(light: light))
  when opts[:clear]  then clear_garbage
  when opts[:deploy] then deploy(host)
  when opts[:apply]  then run_all(host, light: light) do |h|
                            apply_flake(h)
                            activate_home_manager
                          end
  when opts[:boot]  then run_all(host, light: light) do |h|
                           apply_boot_flake(h)
                           activate_home_manager
                         end
  else
    puts "No action specified. Use --help for usage."
    exit 1
  end
end

main(ARGV)

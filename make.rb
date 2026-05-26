#!/usr/bin/env ruby
# frozen_string_literal: true

require 'fileutils'
require 'open3'
require 'optparse'
require 'pathname'
require 'yaml'

# ---------------------------------------------------------------------------
# Config
# ---------------------------------------------------------------------------
_cfg           = File.exist?("config.yml") ? (YAML.safe_load(File.read("config.yml")) || {}) : {}
_theme_cfg     = _cfg["themes"].is_a?(Hash) ? _cfg["themes"] : {}
USER           = _cfg.fetch("user",    ENV.fetch("USER", "wmb")).freeze
SSH_KEY        = _cfg["ssh_key"]&.then { |k| File.expand_path(k) }.freeze
FORGEJO_TOKEN  = _cfg["forgejo_token"]&.then { |t| t.empty? ? nil : t }.freeze
FORGEJO_HOST   = "git.studiowmb.com".freeze
REMOTE_HOSTS   = (_cfg["hosts"] || {}).freeze
DARK_THEME_KEY  = _theme_cfg.fetch("dark", _cfg.fetch("theme", "wilson")).freeze
LIGHT_THEME_KEY = _theme_cfg.fetch("light", "doric-oak").freeze

# ---------------------------------------------------------------------------
# Color palettes
# ---------------------------------------------------------------------------
PALETTES = {
  "doric-valley" => {
    black:         "#383035",
    bg_alt:        "#484040",
    selection:     "#554f52",
    dark_gray:     "#afa497",
    white:         "#e0d5b7",
    red:           "#eca28f",
    olive:         "#c0b060",
    green:         "#b9d0aa",
    blue:          "#a0c0d0",
    mauve:         "#e9acbf",
    cream:         "#f6c097",
    emacs_theme:   "doric-valley",
    ghostty_theme: "Wez",
  },
  "creamsody" => {
    black:         "#1c1a18",
    bg_alt:        "#252320",
    selection:     "#302d29",
    dark_gray:     "#6a6858",
    white:         "#b5b2a0",
    red:           "#884545",
    olive:         "#8a7040",
    green:         "#657050",
    blue:          "#4a6a78",
    mauve:         "#785a5a",
    cream:         "#9a9888",
    emacs_theme:   "creamsody-darker",
    ghostty_theme: "Wez",
  },
  "doric-earth" => {
    black:         "#f0eddf",
    bg_alt:        "#dfdfce",
    selection:     "#d1ceb6",
    dark_gray:     "#635650",
    white:         "#30232e",
    red:           "#a03000",
    olive:         "#705200",
    green:         "#206700",
    blue:          "#74321f",
    mauve:         "#690f44",
    cream:         "#a29986",
    emacs_theme:   "doric-earth",
    ghostty_theme: "Wez",
  },
  "doric-oak" => {
    black:         "#e0d8c7",
    bg_alt:        "#d5c9b5",
    selection:     "#c2b19e",
    dark_gray:     "#6b5225",
    white:         "#3a2018",
    red:           "#982500",
    olive:         "#595000",
    green:         "#226700",
    blue:          "#497020",
    mauve:         "#700054",
    cream:         "#8f9373",
    emacs_theme:   "doric-oak",
    ghostty_theme: "Wez",
  },
  "wilson" => {
    black:         "#222222",
    bg_alt:        "#44443C",
    selection:     "#44443C",
    dark_gray:     "#6C6B59",
    white:         "#BEBFB7",
    red:           "#A56F4B",
    olive:         "#9BA657",
    green:         "#9BA657",
    blue:          "#CFB980",
    mauve:         "#B97E56",
    cream:         "#B9A572",
    emacs_theme:   "wilson",
    ghostty_theme: "Wez",
  },
}.freeze

_palette = PALETTES.fetch(DARK_THEME_KEY, PALETTES["wilson"])

BLACK     = _palette[:black]
BG_ALT    = _palette[:bg_alt]
SELECTION = _palette[:selection]
DARK_GRAY = _palette[:dark_gray]
WHITE     = _palette[:white]
RED       = _palette[:red]
OLIVE     = _palette[:olive]
GREEN     = _palette[:green]
BLUE      = _palette[:blue]
MAUVE     = _palette[:mauve]
CREAM     = _palette[:cream]
THEME_KEY = DARK_THEME_KEY

FONT                = "DejaVu Sans Mono"
TEMPLATES_DIR       = "templates"
ASSETS_DIR          = "assets"
TRANSPARENCY        = "100"
THEME               = _palette[:emacs_theme]
LIGHT_THEME         = "ef-day"
GHOSTTY_THEME       = _palette[:ghostty_theme]
GHOSTTY_THEME_LIGHT = "GruvboxLight"


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
def nix_env
  env = {
    "NIXOS_USER" => USER,
    "NIXOS_THEME" => THEME_KEY,
    "NIXOS_LIGHT_THEME" => LIGHT_THEME_KEY,
  }
  env["NIX_CONFIG"] = "access-tokens = #{FORGEJO_HOST}=#{FORGEJO_TOKEN}" if FORGEJO_TOKEN
  env
end

def prompt(question)
  print "#{question}: "
  $stdout.flush
  gets.chomp
end

def init(host)
  # ── Token / .netrc ──────────────────────────────────────────────────────────
  token = FORGEJO_TOKEN || prompt("Forgejo token")
  abort("!! token required") if token.empty?

  netrc_path = File.expand_path("~/.netrc")
  entry      = "machine #{FORGEJO_HOST}\nlogin #{USER}\npassword #{token}\n"
  existing   = File.exist?(netrc_path) ? File.read(netrc_path) : ""
  cleaned    = existing.lines
    .slice_when { |_, b| b.start_with?("machine ") }
    .reject { |chunk| chunk.first.start_with?("machine #{FORGEJO_HOST}") }
    .flatten.join
  File.write(netrc_path, cleaned + entry)
  File.chmod(0o600, netrc_path)
  puts "~/.netrc configured for #{FORGEJO_HOST}"

  # ── config.yml ──────────────────────────────────────────────────────────────
  config_yml = File.expand_path("config.yml")
  unless File.exist?(config_yml)
    FileUtils.cp(File.expand_path("config.yml.example"), config_yml)
    puts "config.yml created from example — fill in your IPs and token"
  end

  # ── local.nix ───────────────────────────────────────────────────────────────
  local_nix   = File.expand_path("modules/local.nix")
  example_nix = File.expand_path("modules/local.nix.example")

  if File.exist?(local_nix)
    puts "modules/local.nix already exists, skipping"
  else
    FileUtils.cp(example_nix, local_nix)
    puts "modules/local.nix created from example"
    editor = ENV.fetch("EDITOR", nil)
    if editor
      system(editor, local_nix)
    else
      puts "  -> edit modules/local.nix before applying"
    end
  end

  # ── Next steps ──────────────────────────────────────────────────────────────
  puts ""
  puts "Next steps:"
  puts "  1. Edit config.yml       — set host IPs and verify forgejo_token"
  puts "  2. Edit modules/local.nix — add machine-specific packages"
  puts "  3. ruby make.rb --apply #{host || '<host>'}"
end

def setup_netrc
  abort("!! forgejo_token not set in config.yml") unless FORGEJO_TOKEN
  netrc_path = File.expand_path("~/.netrc")
  entry = "machine #{FORGEJO_HOST}\nlogin #{USER}\npassword #{FORGEJO_TOKEN}\n"

  existing = File.exist?(netrc_path) ? File.read(netrc_path) : ""
  cleaned  = existing.lines
    .chunk_while { |a, b| !b.start_with?("machine ") || a.start_with?("machine #{FORGEJO_HOST}") }
    .reject { |chunk| chunk.first.start_with?("machine #{FORGEJO_HOST}") }
    .flatten.join

  File.write(netrc_path, cleaned + entry)
  File.chmod(0o600, netrc_path)
  puts "~/.netrc updated for #{FORGEJO_HOST}"
end

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
  result = run_cmd!(%W[doas nixos-rebuild switch --flake .##{host} --impure], env: nix_env)
  puts "Flake applied on #{host}\n#{result[:out]}"
end

def apply_boot_flake(host)
  result = run_cmd!(%W[doas nixos-rebuild boot --flake .##{host} --impure], env: nix_env)
  puts "Flake applied on boot on #{host}\n#{result[:out]}"
end

def build_iso(host)
  host ||= "workstationiso"
  result = run_cmd!(%W[nix build .#nixosConfigurations.#{host}.config.system.build.isoImage --impure], env: nix_env)
  puts "ISO build done for #{host}\n#{result[:out]}"
end

def build_qcow2(host)
  result = run_cmd!(%W[nix build .#nixosConfigurations.#{host}.config.system.build.qcow2 --impure], env: nix_env)
  puts "qcow2 build done for #{host}\n#{result[:out]}"
end

def build_pi(host)
  result = run_cmd!(%W[nix build .#nixosConfigurations.#{host}.config.system.build.sdImage --impure], env: nix_env)
  puts "Pi image build done for #{host}\n#{result[:out]}"
end

def deploy(host)
  ip = REMOTE_HOSTS.fetch(host) do
    abort("!! Unknown host: #{host} — known: #{REMOTE_HOSTS.keys.join(', ')}")
  end
  env = nix_env.merge(SSH_KEY ? { "NIX_SSHOPTS" => "-i #{SSH_KEY}" } : {})
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
    o.on("-c", "--clear",  "Clear all previous nix store instances")         { opts[:clear]  = true }
    o.on("-a", "--apply",  "Apply flake and templates to HOST")             { opts[:apply]  = true }
    o.on("-t", "--tmpl",   "Build templates only")                          { opts[:tmpl]   = true }
    o.on("-p", "--pi",     "Build SD card image for HOST")                  { opts[:pi]     = true }
    o.on("-i", "--iso",    "Build ISO image for HOST")                      { opts[:iso]    = true }
    o.on("-q", "--qcow2",  "Build qcow2 disk image for HOST")               { opts[:qcow2]  = true }
    o.on("-d", "--deploy", "Deploy HOST via nixos-rebuild --target-host")   { opts[:deploy] = true }
    o.on("-l", "--light",  "Use light theme variant")                       { opts[:light]  = true }
    o.on("-b", "--boot",   "Apply flake with boot flag to HOST")            { opts[:boot]   = true }
    o.on("-n", "--netrc",  "Write Forgejo token to ~/.netrc")               { opts[:netrc]  = true }
    o.on("-I", "--init",   "First-boot setup: .netrc + local.nix")          { opts[:init]   = true }
  end
  rest = parser.parse(argv)
  [opts, rest]
end

def main(argv)
  opts, rest = parse_opts(argv)
  host  = rest.first
  light = opts[:light] || false

  case
  when opts[:init]   then init(host)
  when opts[:netrc]  then setup_netrc
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

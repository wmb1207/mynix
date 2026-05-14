#!/usr/bin/env ruby
# typed: strict
# frozen_string_literal: true

require 'open3'
require 'optparse'
require 'pathname'
require 'sorbet-runtime'

extend T::Sig

# ---------------------------------------------------------------------------
# Color palette — gloomy creamsody
# ---------------------------------------------------------------------------
BLACK     = T.let("#1c1a18", String)
BG_ALT    = T.let("#252320", String)
SELECTION = T.let("#302d29", String)
DARK_GRAY = T.let("#6a6858", String)
WHITE     = T.let("#b5b2a0", String)
RED       = T.let("#884545", String)
OLIVE     = T.let("#8a7040", String)
GREEN     = T.let("#657050", String)
BLUE      = T.let("#4a6a78", String)
MAUVE     = T.let("#785a5a", String)
CREAM     = T.let("#9a9888", String)

FONT                = T.let("DejaVu Sans Mono", String)
TEMPLATES_DIR       = T.let("templates", String)
ASSETS_DIR          = T.let("assets", String)
TRANSPARENCY        = T.let("100", String)
THEME               = T.let("creamsody-darker", String)
LIGHT_THEME         = T.let("ef-day", String)
GHOSTTY_THEME       = T.let("Wez", String)
GHOSTTY_THEME_LIGHT = T.let("GruvboxLight", String)

REMOTE_HOSTS = T.let({
  "nginx"         => "192.168.88.26",
  "podman-server" => "192.168.88.38",
  "coredns"       => "192.168.88.27",
  "postgres"      => "192.168.88.10",
  "vm"            => "192.168.88.37",
  "redis"         => "192.168.88.11",
}.freeze, T::Hash[String, String])

EMACS_PATHS = T.let([
  Pathname("/home/wmb/.emacs.d/init.el"),
  Pathname("/home/wmb/.emacs.d/lisp/packages.el"),
].freeze, T::Array[Pathname])

STAGED_ASSETS = T.let(%w[
  assets/init.el
  assets/bspwmrc
  assets/sxhkdrc
  assets/dunstrc
  assets/polybar.ini
  assets/fvwm3.conf
].freeze, T::Array[String])

# ---------------------------------------------------------------------------
# Data records
# ---------------------------------------------------------------------------
class TemplateField < T::Struct
  const :key,   String
  const :value, String
end

class Template < T::Struct
  const :name,    String
  const :output,  String
  const :content, String
  const :fields,  T::Array[TemplateField]
end

class CmdResult < T::Struct
  const :exit_code, Integer
  const :out,       String
end

# ---------------------------------------------------------------------------
# Shell helpers
# ---------------------------------------------------------------------------
sig { params(cmd: T::Array[String]).returns(CmdResult) }
def run_cmd(cmd)
  puts "Executing: #{cmd.join(' ')}"
  out, status = Open3.capture2e(*cmd)
  CmdResult.new(exit_code: T.must(status.exitstatus), out: out)
end

sig { params(cmd: T::Array[String]).returns(CmdResult) }
def run_cmd!(cmd)
  result = run_cmd(cmd)
  if result.exit_code.zero?
    result
  else
    abort("!! Command failed (exit #{result.exit_code}):\n#{result.out}")
  end
end

# ---------------------------------------------------------------------------
# Template helpers
# ---------------------------------------------------------------------------
sig {
  params(
    name:        String,
    out_file:    String,
    tmpl_file:   String,
    fields_hash: T::Hash[String, String]
  ).returns(Template)
}
def tmpl(name, out_file, tmpl_file, fields_hash)
  Template.new(
    name:    name,
    output:  "#{ASSETS_DIR}/#{out_file}",
    content: File.read("#{TEMPLATES_DIR}/#{tmpl_file}"),
    fields:  fields_hash.map { |k, v| TemplateField.new(key: k, value: v) }
  )
end

sig { params(tmpl: Template).void }
def apply_tmpl(tmpl)
  rendered = tmpl.fields.reduce(tmpl.content) { |acc, f| acc.gsub(f.key, f.value) }
  File.write(tmpl.output, rendered)
  puts "Applied template: #{tmpl.name} -> #{tmpl.output}"
end

sig { params(tmpls: T::Array[Template]).void }
def apply_tmpls!(tmpls)
  puts "Applying templates..."
  tmpls.each { |t| apply_tmpl(t) }
end

# ---------------------------------------------------------------------------
# Template definitions
# ---------------------------------------------------------------------------
sig { returns(Template) }
def polybar_tmpl
  tmpl("polybar", "polybar.ini", "polybar.ini.tmpl", {
    "{{background}}"         => BLACK,
    "{{foreground}}"         => WHITE,
    "{{focused-background}}" => BLUE,
    "{{focused-foreground}}" => BLACK,
    "{{font}}"               => FONT,
  })
end

sig { returns(Template) }
def bspwmrc_tmpl
  tmpl("bspwmrc", "bspwmrc", "bspwmrc.tmpl", {
    "{{active}}"        => "\\#{BLUE}",
    "{{normal-border}}" => "\\#{SELECTION}",
  })
end

sig { returns(Template) }
def sxhkdrc_tmpl
  tmpl("sxhkdrc", "sxhkdrc", "sxhkdrc.tmpl", {
    "{{background}}"          => BLACK,
    "{{font}}"                => FONT,
    "{{selected-foreground}}" => BLACK,
    "{{foreground}}"          => WHITE,
    "{{active}}"              => BLUE,
  })
end

sig { params(light: T::Boolean).returns(Template) }
def ghostty_tmpl(light: false)
  tmpl("ghostty", "ghostty", "ghostty.tmpl", {
    "{{background}}"   => light ? WHITE : BLACK,
    "{{transparency}}" => TRANSPARENCY,
    "{{theme}}"        => light ? GHOSTTY_THEME_LIGHT : GHOSTTY_THEME,
    "{{font}}"         => FONT,
  })
end

sig { returns(Template) }
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
  })
end

sig { returns(Template) }
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

sig { params(light: T::Boolean).returns(Template) }
def emacs_tmpl(light: false)
  tmpl("emacs", "init.el", "init.el.tmpl", {
    "{{transparency}}" => TRANSPARENCY,
    "{{theme}}"        => light ? LIGHT_THEME : THEME,
    "{{font}}"         => FONT,
    "{{background}}"   => light ? WHITE : BLACK,
  })
end

sig { params(light: T::Boolean).returns(T::Array[Template]) }
def all_templates(light: false)
  [polybar_tmpl, bspwmrc_tmpl, sxhkdrc_tmpl, emacs_tmpl(light: light), dunstrc_tmpl, fvwm3_tmpl]
end

# ---------------------------------------------------------------------------
# Operations
# ---------------------------------------------------------------------------
sig { void }
def ensure_sudo!
  result = run_cmd(%w[doas true])
  if result.exit_code.zero?
    puts "Doas authenticated."
  else
    abort("!! Failed to authenticate doas.")
  end
end

sig { params(host: String).void }
def apply_flake(host)
  run_cmd!(%W[doas git config --global --add safe.directory #{Dir.pwd}])
  result = run_cmd!(%W[doas nixos-rebuild switch --flake .##{host} --upgrade --impure])
  puts "Flake applied on #{host}\n#{result.out}"
end

sig { params(host: String).void }
def build_iso(host)
  result = run_cmd!(%W[nix build .#nixosConfigurations.#{host}.config.system.build.isoImage --impure])
  puts "ISO build done for #{host}\n#{result.out}"
end

sig { params(host: String).void }
def build_qcow2(host)
  result = run_cmd!(%W[nix build .#nixosConfigurations.#{host}.config.system.build.qcow2 --impure])
  puts "qcow2 build done for #{host}\n#{result.out}"
end

sig { params(host: String).void }
def build_pi(host)
  result = run_cmd!(%W[nix build .#nixosConfigurations.#{host}.config.system.build.sdImage --impure])
  puts "Pi image build done for #{host}\n#{result.out}"
end

sig { params(host: String).void }
def deploy(host)
  ip = REMOTE_HOSTS.fetch(host) do
    abort("!! Unknown host: #{host} — known: #{REMOTE_HOSTS.keys.join(', ')}")
  end
  result = run_cmd!([
    "nixos-rebuild", "switch",
    "--flake", ".##{host}",
    "--target-host", "wmb@#{ip}",
    "--sudo",
    "--impure",
    "--option", "require-sigs", "false",
  ])
  puts "Deployed #{host} to #{ip}\n#{result.out}"
end

sig { void }
def clear_garbage
  run_cmd!(%w[doas nix-collect-garbage -d])
end

sig { params(path: Pathname).void }
def remove_file(path)
  if path.exist?
    puts "Removing #{path}"
    run_cmd!(%W[doas rm -f #{path}])
    puts "Removed #{path}"
  else
    puts "#{path} not found, skipping."
  end
end

sig { void }
def remove_init_el
  EMACS_PATHS.each { |p| remove_file(p) }
end

sig { void }
def stage_assets
  run_cmd!(["git", "add", *STAGED_ASSETS])
  puts "Assets staged."
end

sig { void }
def activate_home_manager
  activate = T.let(
    "#{ENV.fetch('HOME')}/.local/state/home-manager/gcroots/current-home/activate",
    String
  )
  result = run_cmd!([activate])
  puts "Home-manager activation done.\n#{result.out}"
end

sig { params(host: String, light: T::Boolean).void }
def run_all(host, light: false)
  apply_tmpls!(all_templates(light: light))
  stage_assets
  ensure_sudo!
  remove_init_el
  apply_flake(host)
  activate_home_manager
end

# ---------------------------------------------------------------------------
# CLI
# ---------------------------------------------------------------------------
sig { params(argv: T::Array[String]).returns([T::Hash[Symbol, T::Boolean], T::Array[String]]) }
def parse_opts(argv)
  opts = T.let({}, T::Hash[Symbol, T::Boolean])
  parser = OptionParser.new do |o|
    o.on("-c", "--clear",  "Clear all previous nix store instances") { opts[:clear]  = true }
    o.on("-a", "--apply",  "Apply flake and templates")              { opts[:apply]  = true }
    o.on("-t", "--tmpl",   "Build templates only")                   { opts[:tmpl]   = true }
    o.on("-p", "--pi",     "Build the Pi image")                     { opts[:pi]     = true }
    o.on("-i", "--iso",    "Build the ISO")                          { opts[:iso]    = true }
    o.on("-q", "--qcow2",  "Build a qcow2 disk image")               { opts[:qcow2]  = true }
    o.on("-d", "--deploy", "Deploy to remote host via nixos-rebuild") { opts[:deploy] = true }
    o.on("-l", "--light",  "Use light theme variant")                { opts[:light]  = true }
  end
  rest = T.let(parser.parse(argv.dup), T::Array[String])
  [opts, rest]
end

sig { params(argv: T::Array[String]).void }
def main(argv)
  opts, rest = parse_opts(argv)
  light = T.let(opts[:light] || false, T::Boolean)
  host  = T.let(rest.first, T.nilable(String))

  case
  when opts[:iso]    then build_iso(T.must(host))
  when opts[:pi]     then build_pi(T.must(host))
  when opts[:qcow2]  then build_qcow2(T.must(host))
  when opts[:tmpl]   then apply_tmpls!(all_templates(light: light))
  when opts[:clear]  then clear_garbage
  when opts[:deploy] then deploy(T.must(host))
  when opts[:apply]  then run_all(T.must(host), light: light)
  else
    puts "No action specified. Use --help for usage."
    exit 1
  end
end

main(ARGV)

#!/usr/bin/env ruby
# frozen_string_literal: true

require 'open3'

THEME_DIR = "#{Dir.home}/.config/themes"
FVWM_CMD  = ["/run/current-system/sw/bin/FvwmCommand", "/usr/bin/FvwmCommand"]
              .find { |f| File.exist?(f) }

# ---------------------------------------------------------------------------
# EDN parser — flat keyword->string/vector maps only
# ---------------------------------------------------------------------------
def parse_edn(str)
  theme = {}
  str.scan(/:([a-zA-Z][\w-]*)\s+"([^"]*)"/) { |k, v| theme[k.to_sym] = v }
  str.scan(/:([a-zA-Z][\w-]*)\s+\[([^\]]*)\]/) do |k, vec|
    theme[k.to_sym] = vec.scan(/"([^"]*)"/).flatten
  end
  theme
end

def read_theme(name)
  parse_edn(File.read("#{THEME_DIR}/#{name}.edn"))
end

# ---------------------------------------------------------------------------
# fvwm colorsets
# ---------------------------------------------------------------------------
def fvwm_colorsets(t)
  fg, bg, active, bg_alt, active_alt, olive, comments, selection =
    t.values_at(:foreground, :background, :active,
                :"bg-alt", :"active-alt", :olive, :comments, :selection)
  [
    "Colorset 0  fg #{fg}, bg #{bg},         hi #{bg_alt},     sh #{bg_alt},     Plain, NoShape",
    "Colorset 1  fg #{fg}, bg #{bg},         hi #{bg_alt},     sh #{bg_alt},     Plain, NoShape",
    "Colorset 2  fg #{bg}, bg #{active},     hi #{active_alt}, sh #{active_alt}, Plain, NoShape",
    "Colorset 3  fg #{active_alt}, bg #{active_alt}, hi #{active_alt}, sh #{active_alt}, Plain, NoShape",
    "Colorset 4  fg #{active}, bg #{active}, hi #{active},     sh #{active},     Plain, NoShape",
    "Colorset 5  fg #{fg}, bg #{bg},         hi #{bg_alt},     sh #{bg_alt},     Plain, NoShape",
    "Colorset 6  fg #{bg}, bg #{active},     hi #{active_alt}, sh #{active_alt}, Plain, NoShape",
    "Colorset 7  fg #{comments}, bg #{bg},   hi #{bg_alt},     sh #{bg_alt},     Plain, NoShape",
    "Colorset 8  fg #{bg}, bg #{active_alt}, hi #{active},     sh #{active},     Plain, NoShape",
    "Colorset 10 fg #{fg}, bg #{bg},         hi #{bg_alt},     sh #{bg_alt},     Plain, NoShape",
    "Colorset 11 fg #{bg}, bg #{active},     hi #{active_alt}, sh #{active_alt}, Plain, NoShape",
    "Colorset 12 fg #{bg}, bg #{olive},      hi #{olive},      sh #{olive},      Plain, NoShape",
    "Colorset 13 fg #{bg}, bg #{active_alt}, hi #{active},     sh #{active},     Plain, NoShape",
    "Colorset 14 fg #{comments}, bg #{bg_alt}, hi #{selection}, sh #{selection}, Plain, NoShape",
  ]
end

def apply_fvwm(t)
  unless FVWM_CMD
    puts "⚠ FvwmCommand not found, skipping fvwm update"
    return
  end
  fvwm_colorsets(t).each { |cs| system(FVWM_CMD, cs) }
  system(FVWM_CMD, "Refresh")
rescue => e
  puts "⚠ fvwm: #{e.message}"
end

# ---------------------------------------------------------------------------
# xresources
# ---------------------------------------------------------------------------
def xresources_str(t)
  lines = [
    "URxvt.background:         #{t[:background]}",
    "URxvt.foreground:         #{t[:foreground]}",
    "URxvt.cursorColor:        #{t[:cursor]}",
    "URxvt.highlightColor:     #{t[:highlight]}",
    "URxvt.highlightTextColor: #{t[:"highlight-text"]}",
  ]
  lines += t[:colors].each_with_index.map { |c, i| "URxvt.color#{i}: #{c}" }
  lines.join("\n") + "\n"
end

def apply_xresources(t)
  tmp = "/tmp/theme.Xresources"
  File.write(tmp, xresources_str(t))
  system("xrdb", "-merge", tmp)
end

# ---------------------------------------------------------------------------
# urxvt — live recolor via OSC escape sequences to open pts
# ---------------------------------------------------------------------------
def urxvt_osc(t)
  seqs = [
    "\033]10;#{t[:foreground]}\007",
    "\033]11;#{t[:background]}\007",
    "\033]12;#{t[:cursor]}\007",
    "\033]17;#{t[:highlight]}\007",
    "\033]19;#{t[:"highlight-text"]}\007",
  ]
  seqs += t[:colors].each_with_index.map { |c, i| "\033]4;#{i};#{c}\007" }
  seqs.join
end

def urxvt_ptys
  pids, = Open3.capture2("pgrep urxvt")
  pids.lines.map(&:strip).reject(&:empty?).flat_map do |pid|
    Dir["/proc/#{pid}/fd/*"].filter_map do |fd|
      target = File.realpath(fd) rescue next
      target if target.start_with?("/dev/pts/") && target != "/dev/pts/ptmx"
    end
  end.uniq
rescue
  []
end

def apply_urxvt(t)
  osc = urxvt_osc(t)
  urxvt_ptys.each do |pty|
    File.write(pty, osc)
  rescue => e
    puts "⚠ urxvt #{pty}: #{e.message}"
  end
end

# ---------------------------------------------------------------------------
# tmux
# ---------------------------------------------------------------------------
def apply_tmux(t)
  system("tmux", "set-option", "-g", "status-style",
         "fg=#{t[:foreground]},bg=#{t[:background]}")
  system("tmux", "set-option", "-g", "window-status-current-style",
         "fg=#{t[:background]},bg=#{t[:active]}")
  system("tmux", "refresh-client", "-S")
rescue
  nil
end

# ---------------------------------------------------------------------------
# emacs
# ---------------------------------------------------------------------------
def apply_emacs(t)
  elisp = <<~ELISP.strip
    (progn
      (mapc #'disable-theme custom-enabled-themes)
      (load-theme '#{t[:"emacs-theme"]} t)
      (modify-all-frames-parameters
        '((background-color . "#{t[:background]}")
          (foreground-color . "#{t[:foreground]}")))
      (when (fboundp 'my-reset-whitespace-faces) (my-reset-whitespace-faces))
      (when (fboundp 'my-reset-font) (my-reset-font)))
  ELISP
  system("emacsclient", "--eval", elisp)
rescue
  nil
end

# ---------------------------------------------------------------------------
# main
# ---------------------------------------------------------------------------
def main(argv)
  name = argv.first
  unless %w[dark light].include?(name)
    puts "Usage: theme.rb dark|light"
    exit 1
  end

  t = read_theme(name)
  apply_xresources(t)
  apply_urxvt(t)
  apply_fvwm(t)
  apply_tmux(t)
  apply_emacs(t)
  puts "→ #{name}"
end

main(ARGV)

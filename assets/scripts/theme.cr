#!/usr/bin/env crystal

require "socket"

HOME_DIR  = ENV["HOME"]? || ""
THEME_DIR = File.join(HOME_DIR, ".config/themes")

class Theme
  @colors : Array(String)

  getter colors

  def initialize(@values : Hash(String, String), @vectors : Hash(String, Array(String)))
    @colors = vector("colors")
    raise "Theme key :colors must contain 16 entries" unless @colors.size == 16
  end

  def self.parse(str : String) : Theme
    values = {} of String => String
    vectors = {} of String => Array(String)

    str.scan(/:([a-zA-Z][\w-]*)\s+"([^"]*)"/) do |match|
      values[match[1]] = match[2]
    end

    str.scan(/:([a-zA-Z][\w-]*)\s+\[([^\]]*)\]/m) do |match|
      vectors[match[1]] = match[2].scan(/"([^"]*)"/).map { |item| item[1] }
    end

    new(values, vectors)
  end

  def value(key : String) : String
    @values[key]? || raise "Missing theme key :#{key}"
  end

  private def vector(key : String) : Array(String)
    @vectors[key]? || raise "Missing theme key :#{key}"
  end
end

def theme_path(name : String) : String
  File.join(THEME_DIR, "#{name}.edn")
end

def home_path(*parts : String) : String
  File.join(HOME_DIR, *parts)
end

def fvwm_socket_path : String
  ENV["FVWMMFL_SOCKET"]? ||
    (ENV["TMPDIR"]?.try { |tmpdir| File.join(tmpdir, "fvwm_mfl.sock") }) ||
    "/tmp/fvwm_mfl.sock"
end

def fvwm_cmd(command : String)
  UNIXSocket.open(fvwm_socket_path) do |sock|
    sock << command
    buf = Bytes.new(4096)
    sock.read(buf)
  end
rescue
  nil
end

def read_theme(name : String) : Theme
  Theme.parse(File.read(theme_path(name)))
end

def fvwm_colorsets(t : Theme) : Array(String)
  fg = t.value("foreground")
  bg = t.value("background")
  active = t.value("active")
  bg_alt = t.value("bg-alt")
  active_alt = t.value("active-alt")
  olive = t.value("olive")
  comments = t.value("comments")
  selection = t.value("selection")

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

def apply_fvwm(t : Theme)
  colorsets = fvwm_colorsets(t)
  Dir.mkdir_p(home_path(".fvwm"))
  File.write(home_path(".fvwm", "local.config"), colorsets.join('\n') + "\n")
  colorsets.each { |colorset| fvwm_cmd(colorset) }
  fvwm_cmd("Refresh")
end

def xresources_str(t : Theme) : String
  lines = [
    "URxvt.background:         #{t.value("background")}",
    "URxvt.foreground:         #{t.value("foreground")}",
    "URxvt.borderColor:        #{t.value("background")}",
    "URxvt.cursorColor:        #{t.value("cursor")}",
    "URxvt.highlightColor:     #{t.value("highlight")}",
    "URxvt.highlightTextColor: #{t.value("highlight-text")}",
  ]

  t.colors.each_with_index do |color, index|
    lines << "URxvt.color#{index}: #{color}"
  end

  lines.join('\n') + "\n"
end

def apply_xresources(t : Theme)
  tmp = "/tmp/theme.Xresources"
  File.write(tmp, xresources_str(t))
  system("xrdb", ["-merge", tmp])
rescue
  nil
end

def urxvt_osc(t : Theme) : String
  seqs = [
    "\e]10;#{t.value("foreground")}\a",
    "\e]11;#{t.value("background")}\a",
    "\e]12;#{t.value("cursor")}\a",
    "\e]17;#{t.value("highlight")}\a",
    "\e]19;#{t.value("highlight-text")}\a",
    "\e]708;#{t.value("background")}\a",
  ]

  t.colors.each_with_index do |color, index|
    seqs << "\e]4;#{index};#{color}\a"
  end

  seqs.join
end

def urxvt_ptys : Array(String)
  uid = LibC.getuid.to_s

  Dir.glob("/dev/pts/[0-9]*").select do |path|
    File.info(path).owner_id == uid
  rescue
    false
  end
end

def apply_urxvt(t : Theme)
  osc = urxvt_osc(t)

  urxvt_ptys.each do |pty|
    File.write(pty, osc)
  rescue ex
    puts "warning: urxvt #{pty}: #{ex.message}"
  end
end

def apply_tmux(t : Theme)
  system("tmux", ["set-option", "-g", "status-style", "fg=#{t.value("foreground")},bg=#{t.value("background")}"])
  system("tmux", ["set-option", "-g", "window-status-current-style", "fg=#{t.value("background")},bg=#{t.value("active")}"])
  system("tmux", ["refresh-client", "-S"])
rescue
  nil
end

def apply_emacs(t : Theme)
  elisp = <<-ELISP
    (progn
      (mapc #'disable-theme custom-enabled-themes)
      (load-theme '#{t.value("emacs-theme")} t)
      (modify-all-frames-parameters
        '((background-color . "#{t.value("background")}")
          (foreground-color . "#{t.value("foreground")}")))
      (when (fboundp 'my-reset-whitespace-faces) (my-reset-whitespace-faces))
      (when (fboundp 'my-reset-font) (my-reset-font)))
    ELISP

  system("emacsclient", ["--eval", elisp.strip])
rescue
  nil
end

def apply_wallpaper(t : Theme)
  system("xsetroot", ["-solid", t.value("background")])
rescue
  nil
end

def apply_dmenu(t : Theme)
  opts = "-i -x 151 -lh 3 -fn '#{t.value("font")}' -nb '#{t.value("background")}' -nf '#{t.value("foreground")}' -sb '#{t.value("active")}' -sf '#{t.value("background")}' -bc '#{t.value("foreground")}' -bw 1"
  Dir.mkdir_p(home_path(".config"))
  File.write(home_path(".config", "dmenu.opts"), opts)
end

def available_themes : Array(String)
  Dir.glob(File.join(THEME_DIR, "*.edn")).map { |file| File.basename(file, ".edn") }.sort
end

def usage
  puts "Usage: theme <theme>"
  puts "Available: #{available_themes.join(", ")}"
end

def main(argv : Array(String))
  name = argv.first?

  if name.nil? || !File.exists?(theme_path(name))
    usage
    exit 1
  end

  theme = read_theme(name)
  apply_xresources(theme)
  apply_urxvt(theme)
  apply_fvwm(theme)
  apply_dmenu(theme)
  apply_wallpaper(theme)
  apply_tmux(theme)
  apply_emacs(theme)
  puts "→ #{name}"
rescue ex
  STDERR.puts "theme: #{ex.message}"
  exit 1
end

main(ARGV)

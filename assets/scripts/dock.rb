#!/usr/bin/env ruby
# frozen_string_literal: true

require 'optparse'
require 'open3'

LAPTOP_DISPLAY = "eDP-1"
LAPTOP_MODE    = "2560x1600"

Display = Struct.new(:key, :resolutions, keyword_init: true)
Cmd     = Struct.new(:cmd, :bg, keyword_init: true)

# ---------------------------------------------------------------------------
# xrandr parsing
# ---------------------------------------------------------------------------
def parse_xrandr(input)
  displays = []
  current  = nil
  input.each_line do |line|
    if line =~ / connected/
      displays << current if current
      current = Display.new(key: line.split.first, resolutions: [])
    elsif current && line =~ /^\s+(\d+x\d+)/
      current.resolutions << $1
    end
  end
  displays << current if current
  displays
end

def external_display(displays)
  displays.find { |d| d.key != LAPTOP_DISPLAY }
end

# ---------------------------------------------------------------------------
# command builders
# ---------------------------------------------------------------------------
def xrandr_cmds(position, display)
  base = ["xrandr", "--output", LAPTOP_DISPLAY]
  args = case position
  when "left"     then ["--mode", LAPTOP_MODE, "--left-of",  display.key, "--rotate", "normal",
                        "--output", display.key, "--rotate", "normal", "--mode", display.resolutions.first]
  when "right"    then ["--mode", LAPTOP_MODE, "--right-of", display.key, "--rotate", "normal",
                        "--output", display.key, "--rotate", "normal", "--mode", display.resolutions.first]
  when "bottom"   then ["--mode", LAPTOP_MODE, "--below",    display.key, "--rotate", "normal",
                        "--output", display.key, "--rotate", "normal", "--mode", display.resolutions.first]
  when "top"      then ["--mode", LAPTOP_MODE, "--above",    display.key, "--rotate", "normal",
                        "--output", display.key, "--rotate", "normal", "--mode", display.resolutions.first]
  when "external" then ["--off", "--output", display.key, "--rotate", "normal", "--mode", display.resolutions.first]
  when "laptop"   then ["--mode", LAPTOP_MODE]
  end
  [Cmd.new(cmd: base + args, bg: false)]
end

def bspc_cmds(position, display)
  desktops = %w[first second third fourth fifth]
  case position
  when "external"
    [Cmd.new(cmd: ["bspc", "monitor", LAPTOP_DISPLAY,  "-r"],        bg: false),
     Cmd.new(cmd: ["bspc", "monitor", display.key, "-d", *desktops], bg: false)]
  when "laptop"
    ext_key = display&.key
    [Cmd.new(cmd: ["bspc", "monitor", ext_key,       "-r"],          bg: false),
     Cmd.new(cmd: ["bspc", "monitor", LAPTOP_DISPLAY, "-d", *desktops], bg: false)]
  else
    [Cmd.new(cmd: ["bspc", "monitor", display.key,    "-d", *desktops.first(3)], bg: false),
     Cmd.new(cmd: ["bspc", "monitor", LAPTOP_DISPLAY, "-d", *desktops.last(2)],  bg: false)]
  end
end

def polybar_cmds(position)
  arg = case position
  when "external" then "external"
  when "laptop"   then "laptop"
  else                 "any"
  end
  [Cmd.new(cmd: ["bash", "~/.local/bin/polybar.sh", arg], bg: true)]
end

def keyboard_cmds(keyboard)
  case keyboard
  when "qwerty"
    [Cmd.new(cmd: ["setxkbmap", "-layout", "us", "-variant", "dvorak",
                   "-option", "ctrl:nocaps", "-option", "altwin:meta",
                   "-option", "ctrl:swap_lalt_lctl"], bg: false)]
  when "dvorak"
    [Cmd.new(cmd: ["setxkbmap", "-layout", "us"], bg: false)]
  else
    []
  end
end

# ---------------------------------------------------------------------------
# execution
# ---------------------------------------------------------------------------
def run_cmd(c)
  puts "Executing: #{c.cmd.join(' ')}"
  if c.bg
    Process.detach(Process.spawn(*c.cmd))
  else
    system(*c.cmd)
  end
end

def parse_opts(argv)
  opts = {}
  OptionParser.new do |o|
    o.on("-p", "--position POS", "Display position (left/right/bottom/top/external/laptop)") { |v| opts[:position] = v }
    o.on("-k", "--keyboard KB",  "Keyboard layout (qwerty/dvorak)")                          { |v| opts[:keyboard] = v }
  end.parse(argv)
  opts
end

def main(argv)
  opts     = parse_opts(argv)
  position = opts[:position] || abort("!! --position required")

  xrandr_out, = Open3.capture2("xrandr")
  displays    = parse_xrandr(xrandr_out)
  external    = external_display(displays)

  cmds = [
    *xrandr_cmds(position, external),
    *bspc_cmds(position, external),
    *keyboard_cmds(opts[:keyboard]),
    Cmd.new(cmd: ["bspc", "wm", "-r"], bg: false),
    *polybar_cmds(position),
  ]

  cmds.each { |c| run_cmd(c) }
end

main(ARGV)

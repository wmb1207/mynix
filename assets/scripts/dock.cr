#!/usr/bin/env crystal

LAPTOP_DISPLAY = "eDP-1"
LAPTOP_MODE    = "2560x1600"

struct Display
  getter key, resolutions

  def initialize(@key : String, @resolutions : Array(String))
  end
end

struct Cmd
  getter cmd, bg

  def initialize(@cmd : Array(String), @bg : Bool = false)
  end
end

def parse_xrandr(input : String) : Array(Display)
  displays = [] of Display
  current_key = nil
  current_resolutions = [] of String

  input.each_line do |line|
    if line.includes?(" connected")
      displays << Display.new(current_key, current_resolutions) if current_key
      current_key = line.split.first
      current_resolutions = [] of String
    elsif current_key && (match = line.match(/^\s+(\d+x\d+)/))
      current_resolutions << match[1]
    end
  end

  displays << Display.new(current_key, current_resolutions) if current_key
  displays
end

def external_display(displays : Array(Display)) : Display?
  displays.find { |display| display.key != LAPTOP_DISPLAY }
end

def require_external(display : Display?) : Display
  display || abort("!! external display required")
end

def xrandr_cmds(position : String, display : Display?) : Array(Cmd)
  base = ["xrandr", "--output", LAPTOP_DISPLAY]

  args = case position
         when "left"
           ext = require_external(display)
           ["--mode", LAPTOP_MODE, "--left-of", ext.key, "--rotate", "normal", "--output", ext.key, "--rotate", "normal", "--mode", ext.resolutions.first]
         when "right"
           ext = require_external(display)
           ["--mode", LAPTOP_MODE, "--right-of", ext.key, "--rotate", "normal", "--output", ext.key, "--rotate", "normal", "--mode", ext.resolutions.first]
         when "bottom"
           ext = require_external(display)
           ["--mode", LAPTOP_MODE, "--below", ext.key, "--rotate", "normal", "--output", ext.key, "--rotate", "normal", "--mode", ext.resolutions.first]
         when "top"
           ext = require_external(display)
           ["--mode", LAPTOP_MODE, "--above", ext.key, "--rotate", "normal", "--output", ext.key, "--rotate", "normal", "--mode", ext.resolutions.first]
         when "external"
           ext = require_external(display)
           ["--off", "--output", ext.key, "--rotate", "normal", "--mode", ext.resolutions.first]
         when "laptop"
           ["--mode", LAPTOP_MODE]
         else
           abort("!! unknown position: #{position}")
         end

  [Cmd.new(base + args)]
end

def bspc_cmds(position : String, display : Display?) : Array(Cmd)
  desktops = ["first", "second", "third", "fourth", "fifth"]

  case position
  when "external"
    ext = require_external(display)
    [Cmd.new(["bspc", "monitor", LAPTOP_DISPLAY, "-r"]), Cmd.new(["bspc", "monitor", ext.key, "-d"] + desktops)]
  when "laptop"
    cmds = [] of Cmd
    cmds << Cmd.new(["bspc", "monitor", display.key, "-r"]) if display
    cmds << Cmd.new(["bspc", "monitor", LAPTOP_DISPLAY, "-d"] + desktops)
    cmds
  else
    ext = require_external(display)
    [Cmd.new(["bspc", "monitor", ext.key, "-d"] + desktops.first(3)), Cmd.new(["bspc", "monitor", LAPTOP_DISPLAY, "-d"] + desktops.last(2))]
  end
end

def polybar_cmds(position : String) : Array(Cmd)
  arg = position == "external" ? "external" : position == "laptop" ? "laptop" : "any"
  [Cmd.new(["bash", File.join(ENV["HOME"]? || "", ".local/bin/polybar.sh"), arg], bg: true)]
end

def keyboard_cmds(keyboard : String?) : Array(Cmd)
  case keyboard
  when "qwerty"
    [Cmd.new(["setxkbmap", "-layout", "us", "-variant", "dvorak", "-option", "ctrl:nocaps", "-option", "altwin:meta", "-option", "ctrl:swap_lalt_lctl"])]
  when "dvorak"
    [Cmd.new(["setxkbmap", "-layout", "us"])]
  else
    [] of Cmd
  end
end

def run_cmd(cmd : Cmd)
  puts "Executing: #{cmd.cmd.join(' ')}"
  if cmd.bg
    Process.new(cmd.cmd.first, cmd.cmd[1..])
  else
    system(cmd.cmd.first, cmd.cmd[1..])
  end
end

def opt_value(argv : Array(String), long : String, short : String) : String?
  argv.each_with_index do |arg, index|
    return argv[index + 1]? if arg == long || arg == short
    return arg[(long.size + 1)..] if arg.starts_with?("#{long}=")
  end
end

def main(argv : Array(String))
  position = opt_value(argv, "--position", "-p") || abort("!! --position required")
  keyboard = opt_value(argv, "--keyboard", "-k")

  output = IO::Memory.new
  Process.run("xrandr", output: output)
  displays = parse_xrandr(output.to_s)
  external = external_display(displays)

  cmds = xrandr_cmds(position, external) +
         bspc_cmds(position, external) +
         keyboard_cmds(keyboard) +
         [Cmd.new(["bspc", "wm", "-r"])] +
         polybar_cmds(position)

  cmds.each { |cmd| run_cmd(cmd) }
end

main(ARGV)

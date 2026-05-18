#!/usr/bin/env ruby
# frozen_string_literal: true

require 'optparse'
require 'yaml'
require 'socket'

DRM_EVENT   = /^UDEV\s+\[[\d.]+\]\s+change\s+.*\/drm\//
RESOLUTION  = /(\d+)x(\d+)([+-]\d+)([+-]\d+)/
CONFIG_PATH = File.expand_path("~/.config/screen_setup.yml")

POSITION_FLAGS = {
  "left_of"  => "--left-of",
  "right_of" => "--right-of",
  "above"    => "--above",
  "below"    => "--below",
}.freeze

def fvwm_socket_path
  ENV['FVWMMFL_SOCKET'] ||
    (ENV['TMPDIR'] && "#{ENV['TMPDIR']}/fvwm_mfl.sock") ||
    '/tmp/fvwm_mfl.sock'
end

def fvwm_cmd(command)
  UNIXSocket.open(fvwm_socket_path) do |sock|
    sock.write(command)
    sock.recv(4096)
  end
rescue Errno::ENOENT, Errno::ECONNREFUSED
  nil
end

Display = Struct.new(:name, :max_res, keyword_init: true)

def display_from_array(array)
  name, *rest = array
  res = rest.filter { |r| RESOLUTION.match?(r) }
  Display.new(name: name, max_res: res.first)
end

def connected_displays
  `xrandr | grep " connected"`.split("\n").map { |d| display_from_array(d.split) }
end

def watch_udevadm_monitor
  IO.popen(["udevadm", "monitor", "--subsystem-match=drm", "--udev"]) do |io|
    io.each_line do |line|
      next unless DRM_EVENT.match?(line)
      sleep 2
      yield
    end
  end
end

def load_config
  YAML.safe_load(File.read(CONFIG_PATH)) if File.exist?(CONFIG_PATH)
end

def save_config(config)
  File.write(CONFIG_PATH, YAML.dump(config))
end

# Build and run a single xrandr call covering all connected displays.
# config["outputs"] is a hash of { "OUTPUT-NAME" => { "layout" => "right_of" } }
# "only" is a special layout that turns every other output off.
def apply(config, displays)
  primary = displays.find { |d| d.name == config["primary"] }
  return unless primary

  outputs = config["outputs"] || {}

  # Flatten old single-aux format transparently
  if config["aux"] && config["layout"] && !outputs.key?(config["aux"].to_s)
    outputs = outputs.merge(config["aux"].to_s => { "layout" => config["layout"] })
  end

  args = ["--output", primary.name, "--primary", "--auto"]

  displays.reject { |d| d.name == primary.name }.each do |d|
    layout = outputs.dig(d.name, "layout")
    if layout == "only"
      args += ["--output", d.name, "--off"]
    elsif POSITION_FLAGS.key?(layout.to_s)
      args += ["--output", d.name, "--auto", POSITION_FLAGS[layout], primary.name]
    else
      args += ["--output", d.name, "--auto"]
    end
  end

  system("xrandr", *args)
end

def daemon(config)
  puts "Starting screen_setup daemon"
  apply(config, connected_displays)
  fvwm_cmd("Restart")
  watch_udevadm_monitor do
    system("xrandr", "--auto")
    apply(load_config || config, connected_displays)
    fvwm_cmd("Restart")
  end
end

def parse_opts(argv)
  opts = {}
  OptionParser.new do |o|
    o.on("-d", "--daemon",          "Watch for display events and apply config") { opts[:daemon] = true }
    o.on("-r", "--right-of NAME",   "Place NAME right of primary")  { |v| opts[:output] = v; opts[:layout] = "right_of" }
    o.on("-l", "--left-of NAME",    "Place NAME left of primary")   { |v| opts[:output] = v; opts[:layout] = "left_of"  }
    o.on("-a", "--above NAME",      "Place NAME above primary")     { |v| opts[:output] = v; opts[:layout] = "above"    }
    o.on("-b", "--below NAME",      "Place NAME below primary")     { |v| opts[:output] = v; opts[:layout] = "below"    }
    o.on("-o", "--only NAME",       "Show only NAME, turn off all other displays") { |v| opts[:output] = v; opts[:layout] = "only" }
    o.on("-p", "--primary NAME",    "Set primary display")          { |v| opts[:primary] = v }
  end.parse(argv)
  opts
end

def main(argv)
  opts   = parse_opts(argv)
  config = load_config || { "primary" => "eDP-1", "outputs" => {} }
  config["primary"] = opts[:primary] if opts[:primary]
  config["outputs"] ||= {}

  if opts[:daemon]
    daemon(config)
  elsif opts[:output] && opts[:layout]
    if opts[:layout] == "only"
      # --only NAME: show NAME exclusively, turn off every other display
      config["primary"] = opts[:output]
      connected_displays.reject { |d| d.name == opts[:output] }.each do |d|
        config["outputs"][d.name] = { "layout" => "only" }
      end
      config["outputs"].delete(opts[:output])
    else
      config["outputs"][opts[:output]] = { "layout" => opts[:layout] }
      # If this display was set as sole primary (via --only), restore primary to another display
      if config["primary"] == opts[:output]
        other = connected_displays.find { |d| d.name != opts[:output] }
        if other
          config["primary"] = other.name
          config["outputs"].delete(other.name)
        end
      end
    end
    save_config(config)
    apply(config, connected_displays)
  else
    puts "Usage: screen_setup.rb [--daemon] [--right-of|--left-of|--above|--below|--only NAME] [--primary NAME]"
    exit 1
  end
end

main(ARGV)

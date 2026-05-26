#!/usr/bin/env crystal

require "socket"
require "yaml"

DRM_EVENT   = /^UDEV\s+\[[\d.]+\]\s+change\s+.*\/drm\//
RESOLUTION  = /(\d+)x(\d+)([+-]\d+)([+-]\d+)/
HOME_DIR    = ENV["HOME"]? || ""
CONFIG_PATH = File.join(HOME_DIR, ".config/screen_setup.yml")

POSITION_FLAGS = {
  "left_of"  => "--left-of",
  "right_of" => "--right-of",
  "above"    => "--above",
  "below"    => "--below",
}

struct CLIOption
  getter @name, @long_flag, @short_flag, @description

  def initialize(@name : String, @long_flag : String, @short_flag : String, @description : String)
  end

  def to_tuple
    {{@long_flag, @short_flag} => @name}
  end
  
end

def cli_options : Tuple(Options)
  {
    CLIOption.new("right_of", "--right-of", "-r", "Set the display to the right of the main display"),
    CLIOption.new("left_of", "--left-of", "-l", "Set the display to the left of the main display")
    CLIOption.new("above", "--above", "-a", "Set the display above the main display")
    CLIOption.new("below", "--below", "-b", "Set the display below the main display") 
    CLIOption.new("only", "--only", "-o", "Set the display as the only one")
  }
end

struct Display
  getter name, max_res

  def initialize(@name : String, @max_res : String?)
  end
end

struct ScreenConfig
  property primary, outputs

  def initialize(@primary : String = "eDP-1", @outputs : Hash(String, Hash(String, String)) = {} of String => Hash(String, String))
  end

  def self.from_yaml_file(path : String) : ScreenConfig?
    return nil unless File.exists?(path)

    data = YAML.parse(File.read(path))
    primary = self.parse_primary(data["primary"])
    outputs = self.parse_raw_outputs(data["outputs"])
    

    if aux = data["aux"]?.try(&.as_s?)
      if layout = data["layout"]?.try(&.as_s?)
        outputs[aux] ||= {"layout" => layout}
      end
    end

    new(primary, outputs)
  end

  def to_yaml : String
    {
      "primary" => @primary,
      "outputs" => @outputs,
    }.to_yaml
  end

  private def self.parse_primary(data : YAML.any) : String
    data["primary"]?.try(&.as_s?) || "eDP-1"
  end

  private def self.parse_raw_outputs(data : YAML.any) : Hash(String, String)
    data["outputs"]?.try(&.as_h?).try do |raw|
      raw.compact_map do |name, value|
        layout = value["layout"]?.try(&.as_s?)
        next unless layout
        {name.as_s, {"layout" => layout }}
      end
    end || {} of String
  end
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

def display_from_array(array : Array(String)) : Display
  name = array.first
  res = array[1..].find { |item| RESOLUTION.matches?(item) }
  Display.new(name, res)
end

def connected_displays : Array(Display)
  output = IO::Memory.new
  Process.run("xrandr", output: output)
  output.to_s.lines.select(&.includes?(" connected")).map { |line| display_from_array(line.split) }
end

def watch_udevadm_monitor(&)
  Process.run("udevadm", ["monitor", "--subsystem-match=drm", "--udev"], output: Process::Redirect::Pipe) do |process|
    process.output.each_line do |line|
      next unless DRM_EVENT.matches?(line)

      sleep 2.seconds
      yield
    end
  end
end

def load_config : ScreenConfig
  ScreenConfig.from_yaml_file(CONFIG_PATH) || ScreenConfig.new
end

def save_config(config : ScreenConfig)
  Dir.mkdir_p(File.dirname(CONFIG_PATH))
  File.write(CONFIG_PATH, config.to_yaml)
end

def apply(config : ScreenConfig, displays : Array(Display))
  primary = displays.find { |display| display.name == config.primary }
  return unless primary

  args = ["--output", primary.name, "--primary", "--auto"]

  displays.reject { |display| display.name == primary.name }.reduce(args) do |acc, display|
    layout = config.outputs[display.name]?.try { |entry| entry["layout"]? }

    acc += case
           when layout == "only"
             ["--output", display.name, "--off"]
           when flag = POSITION_FLAGS[layout]?
             ["--output", display.name, "--auto", flag, primary.name]
           else
             ["--output", display.name, "--auto"]
           end
    acc
  end

  system("xrandr", args)
end

def daemon(config : ScreenConfig)
  puts "Starting screen_setup daemon"
  apply(config, connected_displays)
  fvwm_cmd("Restart")

  watch_udevadm_monitor do
    system("xrandr", ["--auto"])
    apply(load_config, connected_displays)
    fvwm_cmd("Restart")
  end
end

def opt_value(argv : Array(String), long : String, short : String) : String?
  argv.each_with_index do |arg, index|
    return argv[index + 1]? if arg == long || arg == short
    return arg[(long.size + 1)..] if arg.starts_with?("#{long}=")
  end
end

def usage
  puts "Usage: screen_setup [--daemon] [--right-of|--left-of|--above|--below|--only NAME] [--primary NAME]"
end

def parse_options
  cli_options.select
end

def main(argv : Array(String))
  config = load_config
  config.primary = opt_value(argv, "--primary", "-p") || config.primary

  output = nil
  layout = nil

  cli_options.map(&.to_tuple).each do |flags, flag_layout|
    if value = opt_value(argv, flags[0], flags[1])
      output = value
      layout = flag_layout
    end
  end

  if argv.includes?("--daemon") || argv.includes?("-d")
    daemon(config)
  elsif output && layout
    displays = connected_displays
    if layout == "only"
      config.primary = output
      displays.reject { |display| display.name == output }.each do |display|
        config.outputs[display.name] = {"layout" => "only"}
      end
      config.outputs.delete(output)
    else
      config.outputs[output] = {"layout" => layout}
      if config.primary == output
        if other = displays.find { |display| display.name != output }
          config.primary = other.name
          config.outputs.delete(other.name)
        end
      end
    end

    save_config(config)
    apply(config, displays)
  else
    usage
    exit 1
  end
end

main(ARGV)

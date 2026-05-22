#!/usr/bin/env crystal

BACKLIGHT_DIR   = "/sys/class/backlight/intel_backlight"
BRIGHTNESS_PATH = File.join(BACKLIGHT_DIR, "brightness")
MAX_PATH        = File.join(BACKLIGHT_DIR, "max_brightness")

def max_brightness : Int32
  File.read(MAX_PATH).strip.to_i
end

def current_brightness : Int32
  File.read(BRIGHTNESS_PATH).strip.to_i
end

def pct_to_abs(pct : Int32) : Int32
  (max_brightness * pct / 100.0).round.to_i
end

def clamp(value : Int32) : Int32
  value.clamp(0, max_brightness)
end

def set_brightness(amount : Int32)
  pct = (amount.to_f / max_brightness * 100).round.to_i
  puts "Setting brightness to #{amount} (#{pct}%)"
  File.write(BRIGHTNESS_PATH, "#{amount}\n")
end

def usage
  puts "Usage: backlight [--increase|-p] [--decrease|-d] [--set|-s N]"
end

def opt_value(argv : Array(String), long : String, short : String) : String?
  argv.each_with_index do |arg, index|
    return argv[index + 1]? if arg == long || arg == short
    return arg[(long.size + 1)..] if arg.starts_with?("#{long}=")
  end
end

def main(argv : Array(String))
  unless File.exists?(BRIGHTNESS_PATH)
    puts "Backlight not found: #{BRIGHTNESS_PATH}"
    exit 1
  end

  cur = current_brightness
  set_value = opt_value(argv, "--set", "-s")

  if argv.includes?("--increase") || argv.includes?("-p")
    set_brightness(clamp(cur + pct_to_abs(10)))
  elsif argv.includes?("--decrease") || argv.includes?("-d")
    set_brightness(clamp(cur - pct_to_abs(10)))
  elsif value = set_value
    set_brightness(clamp(pct_to_abs(value.to_i)))
  else
    usage
    exit 1
  end
end

main(ARGV)

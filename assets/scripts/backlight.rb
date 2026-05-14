#!/usr/bin/env ruby
# frozen_string_literal: true

require 'optparse'

BACKLIGHT_DIR   = "/sys/class/backlight/intel_backlight"
BRIGHTNESS_PATH = "#{BACKLIGHT_DIR}/brightness"
MAX_PATH        = "#{BACKLIGHT_DIR}/max_brightness"

def max_brightness
  File.read(MAX_PATH).strip.to_i
end

def current_brightness
  File.read(BRIGHTNESS_PATH).strip.to_i
end

def pct_to_abs(pct)
  (max_brightness * pct / 100.0).round
end

def clamp(v)
  max = max_brightness
  [[v, 0].max, max].min
end

def set_brightness(amount)
  pct = (amount.to_f / max_brightness * 100).round
  puts "Setting brightness to #{amount} (#{pct}%)"
  File.write(BRIGHTNESS_PATH, "#{amount}\n")
end

def parse_opts(argv)
  opts = {}
  OptionParser.new do |o|
    o.on("-p", "--increase",      "Increase backlight by 10%")      { opts[:increase] = true }
    o.on("-d", "--decrease",      "Decrease backlight by 10%")      { opts[:decrease] = true }
    o.on("-s", "--set N", Integer, "Set backlight to N% (0-100)")   { |v| opts[:set] = v }
  end.parse(argv)
  opts
end

def main(argv)
  unless File.exist?(BRIGHTNESS_PATH)
    puts "Backlight not found: #{BRIGHTNESS_PATH}"
    exit 1
  end

  opts = parse_opts(argv)
  cur  = current_brightness

  case
  when opts[:increase] then set_brightness(clamp(cur + pct_to_abs(10)))
  when opts[:decrease] then set_brightness(clamp(cur - pct_to_abs(10)))
  when opts[:set]      then set_brightness(clamp(pct_to_abs(opts[:set])))
  else
    puts "Usage: backlight.rb [--increase|-p] [--decrease|-d] [--set|-s N]"
    exit 1
  end
end

main(ARGV)

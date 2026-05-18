#!/usr/bin/env ruby
# frozen_string_literal: true

require 'optparse'
require 'socket'

BATTERY_PATH = "/sys/class/power_supply/BAT0/capacity"
BATTERY_STATUS_PATH = "/sys/class/power_supply/BAT0/status"
POWER_SUPPLY_PATH = "/sys/class/power_supply"

def read_percentage(path)
  File.read(path).strip.to_i
end

def list_batteries(dir)
  Dir[dir + "/*/capacity"]
end

def notify_message(percentage)
  "Battery level at #{percentage}/100"
end

def notify(message)
  system("notify-send", message)
rescue => e
  puts "!! Exception during notify-send: #{e.message}"
end

def notify_battery(percentage, state)
  return if percentage == state
  notify(notify_message(percentage)) if percentage < 15 || (percentage % 5).zero?
end

def read_percentages(path)
  list_batteries(path).map { |b| read_percentage(b) }
end

def charging?
  File.exist?(BATTERY_STATUS_PATH) && File.read(BATTERY_STATUS_PATH).strip == "Charging"
end

def panel_label(percentage)
  "bat:#{percentage}%#{charging? ? '+' : ''}"
end

def fvwm_socket_path
  ENV['FVWMMFL_SOCKET'] ||
    (ENV['TMPDIR'] && "#{ENV['TMPDIR']}/fvwm_mfl.sock") ||
    '/tmp/fvwm_mfl.sock'
end

def update_panel(percentage)
  UNIXSocket.open(fvwm_socket_path) do |sock|
    sock.write("SendToModule FvwmButtons ChangeButton battbtn Title #{panel_label(percentage)}")
    sock.recv(4096)
  end
rescue Errno::ENOENT, Errno::ECONNREFUSED
  nil
end

def daemon
  puts "Starting the battery daemon"
  state = nil
  loop do
    percentages = read_percentages(POWER_SUPPLY_PATH)
    percentages.each { |p| notify_battery(p, state) }
    state = percentages.min
    update_panel(state)
    sleep 10
  end
end

def parse_opts(argv)
  opts = {}
  OptionParser.new do |o|
    o.on("-d", "--daemon", "Run the process as a daemon") { opts[:daemon] = true }
    o.on("-n", "--notify", "Trigger the notification")    { opts[:notify] = true }
  end.parse(argv)
  opts
end

def main(argv)
  unless File.exist?(BATTERY_PATH)
    puts "Battery not found at #{BATTERY_PATH}"
    exit 1
  end

  opts = parse_opts(argv)

  case
  when opts[:daemon] then daemon
  when opts[:notify] then read_percentages(POWER_SUPPLY_PATH).each { |p| notify(notify_message(p)) }
  else
    puts "Usage: battery.rb [--daemon|-d] [--notify|-n]"
    exit 1
  end
end

main(ARGV)


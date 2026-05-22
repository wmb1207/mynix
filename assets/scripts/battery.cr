#!/usr/bin/env crystal

require "socket"

BATTERY_PATH        = "/sys/class/power_supply/BAT0/capacity"
BATTERY_STATUS_PATH = "/sys/class/power_supply/BAT0/status"
POWER_SUPPLY_PATH   = "/sys/class/power_supply/"

def read_percentage(path : String) : Int32
  File.read(File.join(path, "capacity")).strip.to_i
end

def list_batteries(dir : String) : Array(String)
  Dir.glob(File.join(dir, "BAT*"))
end

def notify_message(percentage : Int32) : String
  "Battery level at #{percentage}/100"
end

def notify(message : String)
  system("notify-send", [message])
rescue ex
  puts "!! Exception during notify-send: #{ex.message}"
end

def notify_battery(percentage : Int32, state : Int32?)
  return if percentage == state
  notify(notify_message(percentage)) if percentage < 15 || percentage.divisible_by?(5)
end

def read_percentages(path : String) : Array(Int32)
  list_batteries(path).map { |battery| read_percentage(battery) }
end

def charging? : Bool
  File.exists?(BATTERY_STATUS_PATH) && File.read(BATTERY_STATUS_PATH).strip == "Charging"
end

def panel_label(percentage : Int32) : String
  "bat:#{percentage}%#{charging? ? "+" : ""}"
end

def fvwm_socket_path : String
  ENV["FVWMMFL_SOCKET"]? ||
    (ENV["TMPDIR"]?.try { |tmpdir| File.join(tmpdir, "fvwm_mfl.sock") }) ||
    "/tmp/fvwm_mfl.sock"
end

def update_panel(percentage : Int32)
  UNIXSocket.open(fvwm_socket_path) do |sock|
    sock << "SendToModule FvwmButtons ChangeButton battbtn Title #{panel_label(percentage)}"
    buf = Bytes.new(4096)
    sock.read(buf)
  end
rescue
  nil
end

def daemon
  puts "Starting the battery daemon"
  state = nil

  loop do
    percentages = read_percentages(POWER_SUPPLY_PATH)
    unless percentages.empty?
      percentages.each { |percentage| notify_battery(percentage, state) }
      state = percentages.min
      update_panel(state)
    end
    sleep 10.seconds
  end
end

def usage
  puts "Usage: battery [--daemon|-d] [--notify|-n]"
end

def main(argv : Array(String))
  unless File.exists?(BATTERY_PATH)
    puts "Battery not found at #{BATTERY_PATH}"
    exit 1
  end
  
  daemon_opt = argv.includes?("--daemon") || argv.includes?("-d")
  notify_opt = argv.includes?("--notify") || argv.includes?("-n")

  if daemon_opt
    daemon
  elsif notify_opt
    read_percentages(POWER_SUPPLY_PATH).each { |percentage| notify(notify_message(percentage)) }
  else
    usage
    exit 1
  end
end

main(ARGV)

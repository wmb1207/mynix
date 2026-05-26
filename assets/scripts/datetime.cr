#!/usr/bin/env crystal

require "socket"

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

def panel_label : String
  Time.local.to_s("%a %d %H:%M")
end

def update_panel
  fvwm_cmd(%(SendToModule FvwmButtons ChangeButton datetimebtn Title "#{panel_label}"))
end

def daemon
  loop do
    update_panel
    sleep 30.seconds
  end
end

if ARGV.includes?("--daemon") || ARGV.includes?("-d")
  daemon
else
  update_panel
end

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

def current_variant : String?
  output = IO::Memory.new
  Process.run("setxkbmap", ["-query"], output: output)
  output.to_s.lines.find(&.starts_with?("variant:")).try(&.split.last?)
end

def update_panel(variant : String?)
  label = variant == "dvorak" ? "kbd:dv" : "kbd:us"
  fvwm_cmd("SendToModule FvwmButtons ChangeButton kbdbtn Title #{label}")
end

def toggle(variant : String?) : String
  if variant == "dvorak"
    system("setxkbmap", ["-layout", "us", "-variant", "", "-option", "ctrl:nocaps", "-option", "altwin:meta", "-option", "ctrl:swap_lalt_lctl"])
    system("notify-send", ["Switched to QWERTY"])
    "us"
  else
    system("setxkbmap", ["-layout", "us", "-variant", "dvorak", "-option", "ctrl:nocaps", "-option", "altwin:meta", "-option", "ctrl:swap_lalt_lctl"])
    system("notify-send", ["Switched to Dvorak"])
    "dvorak"
  end
end

variant = current_variant

if ARGV.includes?("--sync")
  update_panel(variant)
else
  update_panel(toggle(variant))
end

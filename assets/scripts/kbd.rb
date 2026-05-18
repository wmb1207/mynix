#!/usr/bin/env ruby
# frozen_string_literal: true

require 'socket'

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

def current_variant
  `setxkbmap -query`.lines
    .find { |l| l.start_with?("variant:") }
    &.split&.last&.strip
end

def update_panel(variant)
  label = variant == "dvorak" ? "kbd:dv" : "kbd:us"
  fvwm_cmd("SendToModule FvwmButtons ChangeButton kbdbtn Title #{label}")
end

def toggle(variant)
  if variant == "dvorak"
    system("setxkbmap", "-layout", "us", "-variant", "", "-option", "ctrl:nocaps",
           "-option", "altwin:meta", "-option", "ctrl:swap_lalt_lctl")
    system("notify-send", "Switched to QWERTY")
    "us"
  else
    system("setxkbmap", "-layout", "us", "-variant", "dvorak",
           "-option", "ctrl:nocaps", "-option", "altwin:meta",
           "-option", "ctrl:swap_lalt_lctl")
    system("notify-send", "Switched to Dvorak")
    "dvorak"
  end
end

variant = current_variant

if ARGV.include?("--sync")
  update_panel(variant)
else
  new_variant = toggle(variant)
  update_panel(new_variant)
end

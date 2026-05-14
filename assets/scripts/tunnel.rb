#!/usr/bin/env ruby
# frozen_string_literal: true

require 'optparse'

ENVS = {
  dev: {
    local_port: 0,
    db_host:    "",
    db_port:    0,
    bastion:    "",
    user:       "",
  },
}.freeze

def start_tunnel!(env)
  cfg = ENVS.fetch(env) do
    abort("!! Unknown env: #{env} — available: #{ENVS.keys.join(', ')}")
  end
  cmd = [
    "ssh",
    "-o", "ExitOnForwardFailure=yes",
    "-N",
    "-L", "#{cfg[:local_port]}:#{cfg[:db_host]}:#{cfg[:db_port]}",
    "#{cfg[:user]}@#{cfg[:bastion]}",
  ]
  puts "Tunnel started for #{env} (PID: #{Process.pid})"
  exec(*cmd)
end

def parse_opts(argv)
  opts = {}
  OptionParser.new do |o|
    o.on("-e", "--env ENV", "Environment to tunnel to") { |v| opts[:env] = v.to_sym }
  end.parse(argv)
  opts
end

def main(argv)
  opts = parse_opts(argv)
  abort("!! --env required — available: #{ENVS.keys.join(', ')}") unless opts[:env]
  start_tunnel!(opts[:env])
end

main(ARGV)

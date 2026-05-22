#!/usr/bin/env crystal

struct TunnelConfig
  getter local_port, db_host, db_port, bastion, user

  def initialize(@local_port : Int32, @db_host : String, @db_port : Int32, @bastion : String, @user : String)
  end
end

ENVS = {
  "dev" => TunnelConfig.new(
    local_port: 0,
    db_host: "",
    db_port: 0,
    bastion: "",
    user: ""
  ),
}

def available_envs : String
  ENVS.keys.join(", ")
end

def start_tunnel!(env : String)
  cfg = ENVS[env]? || abort("!! Unknown env: #{env} - available: #{available_envs}")
  cmd = [
    "-o", "ExitOnForwardFailure=yes",
    "-N",
    "-L", "#{cfg.local_port}:#{cfg.db_host}:#{cfg.db_port}",
    "#{cfg.user}@#{cfg.bastion}",
  ]
  puts "Tunnel started for #{env} (PID: #{Process.pid})"
  Process.exec("ssh", cmd)
end

def opt_value(argv : Array(String), long : String, short : String) : String?
  argv.each_with_index do |arg, index|
    return argv[index + 1]? if arg == long || arg == short
    return arg[(long.size + 1)..] if arg.starts_with?("#{long}=")
  end
end

def main(argv : Array(String))
  env = opt_value(argv, "--env", "-e")

  abort("!! --env required - available: #{available_envs}") unless env
  start_tunnel!(env)
end

main(ARGV)

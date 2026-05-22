#!/usr/bin/env crystal

HOME_DIR       = ENV["HOME"]? || ""
TEMPLATE_PATH  = File.join(HOME_DIR, ".local/templates")
SCRIPTS_PATH   = File.join(HOME_DIR, ".local/bin")
SCRIPTS        = ["tunnel"]
TEMPLATE_NAMES = ["php", "python", "nodejs", "raw"]

struct Template
  getter name, template, tags

  def initialize(@name : String, @template : String, @tags : Array(String))
  end
end

TEMPLATES = {
  "php"    => Template.new("php", "php.nix", ["php", "laravel", "nodejs"]),
  "python" => Template.new("python", "python.nix", ["python", "uv", "pip"]),
  "nodejs" => Template.new("nodejs", "nodejs.nix", ["nodejs", "js", "ts"]),
  "raw"    => Template.new("raw", "raw.nix", ["raw", "agnostic"]),
}

def create_project(template : Template, path : String)
  dir = File.expand_path(path)
  Dir.mkdir_p(dir)
  File.copy(File.join(TEMPLATE_PATH, template.template), File.join(dir, template.template))

  SCRIPTS.each do |script|
    src = File.join(SCRIPTS_PATH, script)
    File.copy(src, File.join(dir, script)) if File.exists?(src)
  end

  puts "New dev shell ready to be used"
  puts "Run: cd #{dir}"
end

def opt_value(argv : Array(String), long : String, short : String) : String?
  argv.each_with_index do |arg, index|
    return argv[index + 1]? if arg == long || arg == short
    return arg[(long.size + 1)..] if arg.starts_with?("#{long}=")
  end
end

def main(argv : Array(String))
  path = opt_value(argv, "--path", "-p")
  template_name = opt_value(argv, "--template", "-t")

  abort("!! --path required") unless path
  abort("!! --template required") unless template_name

  template = TEMPLATES[template_name]? || abort("!! Unknown template: #{template_name} - available: #{TEMPLATE_NAMES.join(", ")}")
  create_project(template, path)
end

main(ARGV)

#!/usr/bin/env ruby
# frozen_string_literal: true

require 'optparse'
require 'fileutils'

TEMPLATE_PATH = "#{Dir.home}/.local/templates/"
SCRIPTS_PATH  = "#{Dir.home}/.local/bin/"
SCRIPTS       = %w[tunnel].freeze

Template = Struct.new(:name, :template, :tags, keyword_init: true)

TEMPLATES = {
  "php"    => Template.new(name: "php",    template: "php.nix",    tags: %w[php laravel nodejs]),
  "python" => Template.new(name: "python", template: "python.nix", tags: %w[python uv pip]),
  "nodejs" => Template.new(name: "nodejs", template: "nodejs.nix", tags: %w[nodejs js ts]),
  "raw"    => Template.new(name: "raw",    template: "raw.nix",    tags: %w[raw agnostic]),
}.freeze

def create_project(template, path)
  dir = File.expand_path(path)
  FileUtils.mkdir_p(dir)
  FileUtils.cp(File.join(TEMPLATE_PATH, template.template), dir)
  SCRIPTS.each do |script|
    src = File.join(SCRIPTS_PATH, "#{script}.rb")
    FileUtils.cp(src, dir) if File.exist?(src)
  end
  puts "New dev shell ready to be used"
  puts "Run: cd #{dir}"
end

def parse_opts(argv)
  opts = {}
  OptionParser.new do |o|
    o.on("-n", "--name NAME",     "Project name")                                         { |v| opts[:name]     = v }
    o.on("-t", "--template TMPL", "Base template (#{TEMPLATES.keys.join(', ')})")         { |v| opts[:template] = v }
    o.on("-p", "--path PATH",     "Project root")                                         { |v| opts[:path]     = v }
  end.parse(argv)
  opts
end

def main(argv)
  opts = parse_opts(argv)
  abort("!! --path required") unless opts[:path]
  abort("!! --template required") unless opts[:template]

  tpl = TEMPLATES.fetch(opts[:template]) do
    abort("!! Unknown template: #{opts[:template]} — available: #{TEMPLATES.keys.join(', ')}")
  end

  create_project(tpl, opts[:path])
end

main(ARGV)

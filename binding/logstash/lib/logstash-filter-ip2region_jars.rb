# encoding: utf-8
require "java"

jars = Dir.glob(File.expand_path("../vendor/ip2region-*.jar", __dir__))
unless jars.size == 1
  raise LoadError, "logstash-filter-ip2region expects one vendored ip2region jar, found #{jars.size}: run `bundle exec rake vendor`"
end
require jars.first

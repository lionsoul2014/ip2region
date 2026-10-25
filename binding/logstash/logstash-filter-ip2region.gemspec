Gem::Specification.new do |s|
  s.name          = "logstash-filter-ip2region"
  s.version       = "1.0.0"
  s.licenses      = ["Apache-2.0"]
  s.summary       = "Adds region information to events from ip2region IPv4 and IPv6 xdb files"
  s.description   = "This gem is a Logstash plugin required to be installed on top of the Logstash core pipeline using $LS_HOME/bin/logstash-plugin install gemname. This gem is not a stand-alone program"
  s.authors       = ["The Ip2Region Authors"]
  s.homepage      = "https://github.com/lionsoul2014/ip2region/tree/master/binding/logstash"
  s.require_paths = ["lib"]

  s.files      = Dir["lib/**/*", "spec/**/*", "vendor/**/*", "*.gemspec", "*.md", "Gemfile", "vendor.json"]
  s.test_files = s.files.grep(%r{^spec/})

  # Special flag to let us know this is actually a logstash plugin
  s.metadata = { "logstash_plugin" => "true", "logstash_group" => "filter" }

  s.add_runtime_dependency "logstash-core-plugin-api", "~> 2.0"
  s.add_development_dependency "logstash-devutils", "~> 2"
end

# encoding: utf-8
require "ipaddr"
require "logstash/filters/base"
require "logstash/namespace"
require "logstash-filter-ip2region_jars"

# Looks up the region of the IPv4 or IPv6 address in `source` from ip2region
# xdb files and writes the region fields under `target`.
class LogStash::Filters::Ip2region < LogStash::Filters::Base
  Service = org.lionsoul.ip2region.service.Ip2Region
  XdbConfig = org.lionsoul.ip2region.service.Config

  CACHE_POLICIES = ["file", "vectorIndex", "content"].freeze

  config_name "ip2region"

  # The field containing the IP address to look up.
  config :source, :validate => :string, :required => true

  # The field to write the region fields into.
  config :target, :validate => :string, :default => "ip2region"

  # Path to the IPv4 xdb file. IPv4 addresses are tagged as failures when unset.
  config :v4_database, :validate => :path

  # Path to the IPv6 xdb file. IPv6 addresses are tagged as failures when unset.
  config :v6_database, :validate => :path

  # Cache policy of each xdb file: `file`, `vectorIndex` or `content`.
  config :v4_cache_policy, :validate => CACHE_POLICIES, :default => "vectorIndex"
  config :v6_cache_policy, :validate => CACHE_POLICIES, :default => "vectorIndex"

  # Number of pooled searchers per xdb file for the `file` and `vectorIndex`
  # cache policies. Size it to at least the pipeline worker count.
  config :searchers, :validate => :number, :default => 20

  # Field names for the `|`-separated region columns, by position.
  config :fields, :validate => :array, :default => ["country", "province", "city", "isp", "country_code"]

  # Tags added when the address is invalid or has no region.
  config :tag_on_failure, :validate => :array, :default => ["_ip2region_lookup_failure"]

  def register
    if @v4_database.nil? && @v6_database.nil?
      raise LogStash::ConfigurationError, "ip2region: set `v4_database`, `v6_database` or both"
    end

    target = @target.start_with?("[") ? @target : "[#{@target}]"
    @field_refs = @fields.map { |name| "#{target}[#{name}]" }

    v4_config = @v4_database && xdb_config(@v4_database, @v4_cache_policy).asV4
    v6_config = @v6_database && xdb_config(@v6_database, @v6_cache_policy).asV6
    @service = Service.java_send(:create, [XdbConfig, XdbConfig], v4_config, v6_config)
  end

  def filter(event)
    ip = event.get(@source)
    return if ip.nil?

    region = lookup(ip.to_s)
    if region.nil? || region.empty?
      @tag_on_failure.each { |tag| event.tag(tag) }
      return
    end

    @field_refs.zip(region.split("|", -1)).each do |ref, value|
      # ip2region stores "0" for an unknown column
      next if value.nil? || value.empty? || value == "0"
      event.set(ref, value)
    end

    filter_matched(event)
  end

  def close
    @service.close
  end

  private

  def xdb_config(path, cache_policy)
    XdbConfig.custom
      .setXdbPath(path)
      .setCachePolicy(XdbConfig.cachePolicyFromName(cache_policy))
      .setSearchers(@searchers)
  end

  # Parses the address with IPAddr so that hostnames never reach the
  # Java parser, which resolves them through DNS.
  def lookup(ip)
    # IPAddr also parses networks such as 1.2.3.0/24
    raise IPAddr::InvalidAddressError, "a network, not an address" if ip.include?("/")

    address = IPAddr.new(ip).native
    @service.search(address.hton.to_java_bytes)
  rescue IPAddr::Error => e
    logger.debug("ip2region: invalid IP address", :source => @source, :value => ip, :message => e.message)
    nil
  end
end

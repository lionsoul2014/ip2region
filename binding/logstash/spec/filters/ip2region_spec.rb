# encoding: utf-8
require "logstash/devutils/rspec/spec_helper"
require "logstash/filters/ip2region"

describe LogStash::Filters::Ip2region do
  data_dir = File.expand_path("../../../../data", __dir__)
  v4_database = File.join(data_dir, "ip2region_v4.xdb")
  v6_database = File.join(data_dir, "ip2region_v6.xdb")

  describe "with both xdb files" do
    config <<-CONFIG
      filter {
        ip2region {
          source => "ip"
          v4_database => "#{v4_database}"
          v6_database => "#{v6_database}"
          add_tag => ["located"]
        }
      }
    CONFIG

    sample({ "ip" => "1.2.3.4" }) do
      expect(subject.get("ip2region")).to eq(
        "country" => "Australia",
        "province" => "Queensland",
        "city" => "Brisbane",
        "country_code" => "AU"
      )
      expect(subject.get("tags")).to eq(["located"])
    end

    sample({ "ip" => "240e:3b7:3272:d8d0:db09:c067:8d59:539e" }) do
      expect(subject.get("ip2region")).to eq(
        "country" => "中国",
        "province" => "广东省",
        "city" => "深圳市",
        "isp" => "电信",
        "country_code" => "CN"
      )
    end

    sample({ "ip" => "::ffff:1.2.3.4" }) do
      expect(subject.get("[ip2region][country]")).to eq("Australia")
    end

    ["not-an-ip", "localhost", "1.2.3", "1.2.3.4/24", ""].each do |value|
      sample({ "ip" => value }) do
        expect(subject.get("ip2region")).to be_nil
        expect(subject.get("tags")).to eq(["_ip2region_lookup_failure"])
      end
    end

    sample({ "message" => "no address" }) do
      expect(subject.get("ip2region")).to be_nil
      expect(subject.get("tags")).to be_nil
    end
  end

  ["file", "vectorIndex", "content"].each do |cache_policy|
    describe "with the #{cache_policy} cache policy" do
      config <<-CONFIG
        filter {
          ip2region {
            source => "ip"
            v4_database => "#{v4_database}"
            v6_database => "#{v6_database}"
            v4_cache_policy => "#{cache_policy}"
            v6_cache_policy => "#{cache_policy}"
          }
        }
      CONFIG

      sample({ "ip" => "1.2.3.4" }) do
        expect(subject.get("[ip2region][city]")).to eq("Brisbane")
      end

      sample({ "ip" => "240e:3b7:3272:d8d0:db09:c067:8d59:539e" }) do
        expect(subject.get("[ip2region][city]")).to eq("深圳市")
      end
    end
  end

  describe "with a custom target, fields and failure tag" do
    config <<-CONFIG
      filter {
        ip2region {
          source => "[client][ip]"
          target => "[client][geo]"
          v4_database => "#{v4_database}"
          fields => ["country_name", "region_name"]
          tag_on_failure => ["_no_region"]
        }
      }
    CONFIG

    sample({ "client" => { "ip" => "1.2.3.4" } }) do
      expect(subject.get("[client][geo]")).to eq("country_name" => "Australia", "region_name" => "Queensland")
    end

    sample({ "client" => { "ip" => "240e:3b7:3272:d8d0:db09:c067:8d59:539e" } }) do
      expect(subject.get("[client][geo]")).to be_nil
      expect(subject.get("tags")).to eq(["_no_region"])
    end
  end

  describe "without any xdb file" do
    it "refuses to register" do
      filter = described_class.new("source" => "ip")
      expect { filter.register }.to raise_error(LogStash::ConfigurationError, /v4_database/)
    end
  end

  describe "with an xdb file of the wrong IP version" do
    it "refuses to register" do
      filter = described_class.new("source" => "ip", "v4_database" => v6_database)
      expect { filter.register }.to raise_error(/ip verison not match/)
    end
  end
end

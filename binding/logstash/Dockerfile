# Tests the plugin in the official Logstash image. Build it from the
# repository root:
#   docker build --file binding/logstash/Dockerfile .
ARG LOGSTASH_VERSION=9.5.5

FROM docker.elastic.co/logstash/logstash:${LOGSTASH_VERSION} AS test
SHELL ["/bin/bash", "-o", "pipefail", "-c"]
ENV LOGSTASH_PATH=/usr/share/logstash \
    LOGSTASH_SOURCE=1
WORKDIR /usr/share/logstash/ip2region/binding/logstash
COPY --chown=logstash:root data/ip2region_v4.xdb data/ip2region_v6.xdb ../../data/
COPY --chown=logstash:root binding/logstash ./
RUN "$LOGSTASH_PATH/bin/ruby" -S bundle install \
    && "$LOGSTASH_PATH/bin/ruby" -S bundle exec rake vendor \
    && "$LOGSTASH_PATH/bin/ruby" -S bundle exec rspec

# Installs the gem with logstash-plugin and looks up addresses in a pipeline.
FROM test AS pipeline
RUN "$LOGSTASH_PATH/bin/ruby" "$LOGSTASH_PATH/vendor/jruby/bin/gem" build logstash-filter-ip2region.gemspec \
    && "$LOGSTASH_PATH/bin/logstash-plugin" install "$PWD"/logstash-filter-ip2region-*.gem
RUN "$LOGSTASH_PATH/bin/logstash" --log.level=error --pipeline.workers=2 -e " \
      input { generator { lines => ['1.2.3.4', '240e:3b7:3272:d8d0:db09:c067:8d59:539e'] count => 1 } } \
      filter { ip2region { source => 'message' v4_database => '$PWD/../../data/ip2region_v4.xdb' v6_database => '$PWD/../../data/ip2region_v6.xdb' } } \
      output { stdout { codec => json_lines } }" \
    | tee events.ndjson \
    && grep -q '"country_code":"AU"' events.ndjson \
    && grep -q '"country_code":"CN"' events.ndjson

# 只实现了 memory 版本的， 其他的靠兄弟们实现了

require_relative 'ip_2_region'

s = Ip2Region.new

(1..235).each do |i|
  ip = "#{i}.#{i}.#{i}.#{i}"
  answer = s.memorySearch ip
  puts "#{ip} , answer = #{answer}"
end

ip = "27.10.228.9"
answer = s.memorySearch ip
puts "#{ip} , answer = #{answer}"
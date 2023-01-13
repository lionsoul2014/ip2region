-module(util).
-export([ipv4_to_n/1]).


ip_aton(Ip) ->
	{ok, Addr} = inet_parse:address(Ip),
	Addr.

ipv4_to_n({A, B, C, D}) ->
	<<N:32>> = <<A, B, C, D>>,
	N;
ipv4_to_n(Ip) when is_binary(Ip) ->
	ipv4_to_n(binary_to_list(Ip));
ipv4_to_n(Ip) when is_list(Ip) ->
	Addr = ip_aton(Ip),
	ipv4_to_n(Addr).
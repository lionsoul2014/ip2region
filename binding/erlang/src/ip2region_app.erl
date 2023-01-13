%%%-------------------------------------------------------------------
%% @doc ip2region public API
%% @end
%%%-------------------------------------------------------------------

-module(ip2region_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    ip2region_sup:start_link().

stop(_State) ->
    ok.

%% internal functions

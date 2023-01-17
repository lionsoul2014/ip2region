%%%-------------------------------------------------------------------
%% Copyright 2022 The Ip2Region Authors. All rights reserved.
%% Use of this source code is governed by a Apache2.0-style
%% license that can be found in the LICENSE file.
%% 
%% @doc ip2region top level supervisor.
%% @end
%%%-------------------------------------------------------------------
-module(ip2region_sup).
-behaviour(supervisor).
-include("ip2region.hrl").

-export([start_link/0]).

-export([init/1, create_table/0]).

-define(SERVER, ?MODULE).

start_link() ->
    {ok, SupPid} = supervisor:start_link({local, ?SERVER}, ?MODULE, []),
    {ok, _PoolPid} = start_ip2region_pool(SupPid),
    {ok, SupPid}.

%% sup_flags() = #{strategy => strategy(),         % optional
%%                 intensity => non_neg_integer(), % optional
%%                 period => pos_integer()}        % optional
%% child_spec() = #{id => child_id(),       % mandatory
%%                  start => mfargs(),      % mandatory
%%                  restart => restart(),   % optional
%%                  shutdown => shutdown(), % optional
%%                  type => worker(),       % optional
%%                  modules => modules()}   % optional
init([]) ->
    create_table(),
    SupFlags = #{strategy => one_for_one,
                 intensity => 10,
                 period => 5},
    ChildSpecs = [],
    {ok, {SupFlags, ChildSpecs}}.

%% internal functions
%% 
create_table() ->
    Opts = [named_table, set, public, {read_concurrency, true}, {keypos, 1}],
    ets:new(?XDB_VECTOR_INDEX, Opts),
    ets:new(?XDB_SEGMENT_INDEX, Opts),
    ets:new(?IP2REGION_CACHE, Opts).

start_ip2region_pool(Sup) ->
    {ok, PoolArgsCfg} = application:get_env(poolargs),
    PoolName = ?IP2REGION_POOL,
    PoolArgs = [{strategy, fifo}, {name, {local, PoolName}}, {worker_module, ip2region_worker} | PoolArgsCfg],
    WorkerArgs = [],
    ChildSpecs = poolboy:child_spec(PoolName, PoolArgs, WorkerArgs),
    supervisor:start_child(Sup, ChildSpecs).
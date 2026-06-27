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
    ChildSpecs = pool_child_specs(),
    {ok, {SupFlags, ChildSpecs}}.

%% internal functions
%%
create_table() ->
    Opts = [named_table, set, public, {read_concurrency, true}, {keypos, 1}],
    %% Version-specific tables for dual-stack support
    ensure_table(?XDB_VECTOR_INDEX_V4, Opts),
    ensure_table(?XDB_VECTOR_INDEX_V6, Opts),
    ensure_table(?XDB_SEGMENT_INDEX_V4, Opts),
    ensure_table(?XDB_SEGMENT_INDEX_V6, Opts),
    ensure_table(?IP2REGION_CACHE_V4, Opts),
    ensure_table(?IP2REGION_CACHE_V6, Opts).

ensure_table(Name, Opts) ->
    case ets:whereis(Name) of
        undefined -> ets:new(Name, Opts);
        _ -> ok
    end.

pool_child_specs() ->
    {ok, DbConfig} = application:get_env(db),
    {ok, PoolArgsCfg} = application:get_env(poolargs),
    lists:foldl(
        fun({ipv4, File}, Acc) ->
                [make_pool_spec(?IP2REGION_POOL_V4, ipv4, File, PoolArgsCfg) | Acc];
           ({ipv6, File}, Acc) ->
                [make_pool_spec(?IP2REGION_POOL_V6, ipv6, File, PoolArgsCfg) | Acc];
           (_, Acc) ->
                Acc
        end, [], DbConfig).

make_pool_spec(PoolName, Version, File, PoolArgsCfg) ->
    PoolArgs = [
        {strategy, fifo},
        {name, {local, PoolName}},
        {worker_module, ip2region_worker}
        | PoolArgsCfg
    ],
    WorkerArgs = [{xdb_file, File}, {version, Version}],
    poolboy:child_spec(PoolName, PoolArgs, WorkerArgs).

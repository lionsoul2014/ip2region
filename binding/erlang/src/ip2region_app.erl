%%%-------------------------------------------------------------------
%% Copyright 2022 The Ip2Region Authors. All rights reserved.
%% Use of this source code is governed by a Apache2.0-style
%% license that can be found in the LICENSE file.
%% 
%% @doc 
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

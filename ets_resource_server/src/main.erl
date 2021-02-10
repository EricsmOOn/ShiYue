%%%-------------------------------------------------------------------
%%% @author ericwong
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 09. Feb 2021 00:29
%%%-------------------------------------------------------------------
-module(main).
-author("ericwong").

%% API
-export([start/0]).

start() ->
  ets_resource_server:start_link(),
  ets_resource_client:start(),
  ets_resource_client:start(),
  ets_resource_client:start().
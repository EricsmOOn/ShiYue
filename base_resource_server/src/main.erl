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
  base_resource_server:start_link(),
  base_resource_client:start(),
  base_resource_client:start(),
  base_resource_client:start().
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
  log_resource_server:start_link(),
  log_resource_client:start(),
  log_resource_client:start(),
  log_resource_client:start(),
  sleep(20000),
  log_resource_server:stop().

sleep(T) ->
  receive
  after T ->
    true
  end.
%%%-------------------------------------------------------------------
%%% @author ericwong
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 06. Feb 2021 21:30
%%%-------------------------------------------------------------------
-module(base_resource_server).
-author("ericwong").

-behaviour(gen_server).

-import(base_resource_client, [resource_already_release/2]).

%% API
-export([start_link/0, request_resource/2, return_resource/1]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).

-define(TOTAL, 20).   %% 持有资源总数

-record(state, {}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link() ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

request_resource(Amount, Time) -> gen_server:call(?MODULE, {request_resource, Amount, Time}).

return_resource(Key) -> gen_server:call(?MODULE, {return_resource, Key}).


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
-spec(init(Args :: term()) ->
  {ok, State :: #state{}} | {ok, State :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term()} | ignore).
init([]) ->
  io:format("Server[~p]:online~n", [base_resource_server]),
  process_flag(trap_exit, true),
  {ok, {?TOTAL, dict:new()}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #state{}) ->
  {reply, Reply :: term(), NewState :: #state{}} |
  {reply, Reply :: term(), NewState :: #state{}, timeout() | hibernate} |
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), Reply :: term(), NewState :: #state{}} |
  {stop, Reason :: term(), NewState :: #state{}}).
handle_call({request_resource, Amount, Time}, From, {Total, Dict}) ->
  Reply = if
            Amount > Total ->
              NewState = {Total, Dict},
              io:format("Server[~p]:{Reply,~p},{Total,~p}~n", [base_resource_server, no_enough_resource, Total]),
              {error, no_enough_resource};
            true ->
              {Key, NewDict} = lend_resource(Amount, Time, From, Dict),
              NewState = {Total - Amount, NewDict},
              erlang:send_after(Time, self(), {resource_time_out, Key}),
              io:format("Server[~p]:{Reply,~p},{Total,~p}~n", [base_resource_server, {ok, Key}, Total - Amount]),
              {ok, Key}
          end,
  {reply, Reply, NewState};

handle_call({return_resource, Key}, _From, {Total, Dict}) ->
  Reply = case dict:find(Key, Dict) of
            {ok, {Amount, _Time, _Name}} ->
              NewTotal = Total + Amount,
              NewDict = dict:erase(Key, Dict),
              io:format("Server[~p]:{~p,~p},{Total,~p}~n", [base_resource_server, receive_return_resource, Key, NewTotal]),
              {ok, return_succeed};
            error ->
              NewTotal = Total,
              NewDict = Dict,
              io:format("Server[~p]:{~p,~p},{Total,~p}~n", [base_resource_server, resource_already_release, Key, NewTotal]),
              {error, resource_already_release}
          end,
  {reply, Reply, {NewTotal, NewDict}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_cast(Request :: term(), State :: #state{}) ->
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #state{}}).
handle_cast(_Request, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
-spec(handle_info(Info :: timeout() | term(), State :: #state{}) ->
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #state{}}).
handle_info({resource_time_out, Key}, {Total, Dict}) ->
  {NewTotal, NewDict} =
    case dict:find(Key, Dict) of
      {ok, {Amount, _Time, Name}} ->
        Dict1 = dict:erase(Key, Dict),
        resource_already_release(Name, Key),
        io:format("Server[~p]:{~p,~p},{Total,~p}~n", [base_resource_server, resource_time_out, Key, Total + Amount]),
        {Total + Amount, Dict1};
      _ ->
        io:format("Server[~p]:{~p,~p},{Total,~p}~n", [base_resource_server, resource_already_release, Key, Total]),
        {Total, Dict}
    end,
  {noreply, {NewTotal, NewDict}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #state{}) -> term()).
terminate(_Reason, _State) ->
  ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #state{},
    Extra :: term()) ->
  {ok, NewState :: #state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
lend_resource(Amount, Time, From, Dict) ->
  Key = rand:uniform(9999),
  {Pid, _} = From,
  NewDict = dict:store(Key, {Amount, Time, Pid}, Dict),
  {Key, NewDict}.

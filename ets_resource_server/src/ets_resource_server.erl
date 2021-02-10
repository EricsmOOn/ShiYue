%%%-------------------------------------------------------------------
%%% @author ericwong
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 06. Feb 2021 21:30
%%%-------------------------------------------------------------------
-module(ets_resource_server).
-author("ericwong").

-behaviour(gen_server).

-import(ets_resource_client, [resource_already_release/2]).

%% API
-export([start_link/0, stop/0, request_resource/2, return_resource/1]).

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
stop() ->
  gen_server:call(?MODULE, stop).

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
  Ets = init_dets(),
  io:format("Server[~p]:online~n", [?MODULE]),
  process_flag(trap_exit, true),
  {ok, {?TOTAL, Ets}}.

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
handle_call({request_resource, Amount, Time}, From, {Total, Ets}) ->
  Reply = if
            Amount > Total ->
              NewState = {Total, Ets},
              io:format("Server[~p]:{Reply,~p},{Total,~p}~n", [?MODULE, no_enough_resource, Total]),
              {error, no_enough_resource};
            true ->
              {Key, NewEts} = lend_resource(Amount, Time, From, Ets),
              NewState = {Total - Amount, NewEts},
              erlang:send_after(Time, self(), {resource_time_out, Key}),
              io:format("Server[~p]:{Reply,~p},{Total,~p}~n", [?MODULE, {ok, Key}, Total - Amount]),
              {ok, Key}
          end,
  {reply, Reply, NewState};

handle_call({return_resource, Key}, _From, {Total, Ets}) ->
  Reply = case ets:lookup(Ets, Key) of
            [{Key, {Start, Amount, Time, Pid, running}}] ->
              NewTotal = Total + Amount,
              ets:insert(Ets, {Key, {Start, Amount, Time, Pid, finished}}),
              io:format("Server[~p]:{~p,~p},{Total,~p}~n", [?MODULE, receive_return_resource, Key,
                NewTotal]),
              {ok, return_succeed};
            [{_, {_, _, _, _, finished}}] ->
              NewTotal = Total,
              io:format("Server[~p]:{~p,~p},{Total,~p}~n", [?MODULE, resource_already_release, Key,
                NewTotal]),
              {error, resource_already_release};
            Other ->
              NewTotal = Total,
              error(Other)
          end,
  {reply, Reply, {NewTotal, Ets}}.

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
handle_info({resource_time_out, Key}, {Total, Ets}) ->
  NewTotal =
    case ets:lookup(Ets, Key) of
      [{Key, {Start, Amount, Time, Pid, running}}] ->
        ets:insert(Ets, {Key, {Start, Amount, Time, Pid, finished}}),
        resource_already_release(Pid, Key),
        io:format("Server[~p]:{~p,~p},{Total,~p}~n", [?MODULE, resource_time_out, Key, Total + Amount]),
        Total + Amount;
      [{_, {_, _, _, _, finished}}] ->
        io:format("Server[~p]:{~p,~p},{Total,~p}~n", [?MODULE, resource_already_release, Key, Total]),
        Total;
      Other ->
        error(Other)
    end,
  {noreply, {NewTotal, Ets}}.

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
terminate(_Reason, {_Total, Ets}) ->
  File = "./dets/dets",
  {ok, ?MODULE} = dets:open_file(?MODULE, [{file, File}]),
  io:format("~p~n", [ets:tab2list(Ets)]),
  ets:to_dets(Ets, ?MODULE),
  dets:close(?MODULE),
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
lend_resource(Amount, Time, From, Ets) ->
%%  io:format("~p~n", [ets:last(Ets)]),
  Key = ets:last(Ets) + 1,
  {Pid, _} = From,
  ets:insert_new(Ets, {Key, {erlang:system_time(), Amount, Time, Pid, running}}),
  {Key, Ets}.

init_dets() ->
%%  io:format("~p~n", [file:list_dir(".")]),
  File = "./dets/dets",
  Bool = filelib:is_file(File),
  Ets = ets:new(ets_table, [ordered_set, protected, {keypos, 1}]),
  ets:insert_new(Ets, {0, {erlang:system_time(), ?TOTAL, init}}),
  case dets:open_file(?MODULE, [{file, File}]) of
    {ok, ?MODULE} ->
      case Bool of
        true ->
          dets:to_ets(?MODULE, Ets),
          io:format("~p~n", [ets:tab2list(Ets)]);
        false -> ok = dets:insert(?MODULE, {0, init})
      end;
    {error, Reason} ->
      error(Reason)
  end,
  dets:close(?MODULE),
  Ets.

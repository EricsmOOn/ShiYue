%%%-------------------------------------------------------------------
%%% @author ericwong
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 06. Feb 2021 21:30
%%%-------------------------------------------------------------------
-module(log_resource_server).
-author("ericwong").

-behaviour(gen_server).

-import(log_resource_client, [resource_already_release/2]).

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
  gen_server:cast(?MODULE, stop).

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
  io:format("[~s] - Server[~20w]:online~n", [get_now_time(), ?MODULE]),
  Log = io_lib:format("[~20s] - Server[~20w]:[sys_info]Server Online.~n", [get_now_time(), ?MODULE]),
  log(Log),
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
              io:format("[~s] - Server[~20w]:{Reply,~p},{Total,~p}~n", [get_now_time(), ?MODULE, no_enough_resource,
                Total]),
              Log = io_lib:format("[~20s] - Server[~20w]:[  cancel]Resource is not enough,Only ~p left.~n",
                [get_now_time(), ?MODULE, Total]),
              log(Log),
              {error, no_enough_resource};
            true ->
              {Key, NewEts} = lend_resource(Amount, Time, From, Ets),
              NewState = {Total - Amount, NewEts},
              erlang:send_after(Time, self(), {resource_time_out, Key}),
              io:format("[~s] - Server[~20w]:{Reply,~p},{Total,~p}~n", [get_now_time(), ?MODULE, {ok, Key}, Total -
                Amount]),
              {ok, Key}
          end,
  {reply, Reply, NewState};

handle_call({return_resource, Key}, _From, {Total, Ets}) ->
  Reply = case ets:lookup(Ets, Key) of
            [{Key, {Start, Amount, Time, Pid, running}}] ->
              NewTotal = Total + Amount,
              ets:insert(Ets, {Key, {Start, Amount, Time, Pid, finished}}),
              io:format("[~s] - Server[~20w]:{~p,~p},{Total,~p}~n", [get_now_time(), ?MODULE, receive_return_resource,
                Key,
                NewTotal]),
              Log = io_lib:format("[~20s] - Server[~20w]:[finished]Task ~p return {~w return ~w resource} ~w resource left.~n",
                [get_now_time(), ?MODULE, Key,Pid,Amount,NewTotal]),
              log(Log),
              {ok, return_succeed};
            [{_, {_, Amount, _, Pid, finished}}] ->
              NewTotal = Total,
              io:format("[~s] - Server[~20w]:{~p,~p},{Total,~p}~n", [get_now_time(), ?MODULE, resource_already_release,
                Key,
                NewTotal]),
              Log = io_lib:format("[~20s] - Server[~20w]:[  cancel]Task ~p finished {~w return ~w resource} but resource already release.~n"
                , [get_now_time(), ?MODULE, Key,Pid,Amount]),
              log(Log),
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
handle_cast(stop, State) ->
  {stop, normal, State}.

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
        io:format("[~s] - Server[~20w]:{~p,~p},{Total,~p}~n", [get_now_time(), ?MODULE, resource_time_out, Key, Total +
          Amount]),
        Log = io_lib:format("[~20s] - Server[~20w]:[finished]Task ~p timeout {~w resource ~w ms timeout} ~w resource left.~n",
          [get_now_time(), ?MODULE, Key,Amount,Time,Total + Amount]),
        log(Log),
        Total + Amount;
      [{_, {_, Amount, _, Pid, finished}}] ->
        io:format("[~s] - Server[~20w]:{~p,~p},{Total,~p}~n", [get_now_time(), ?MODULE, resource_already_release, Key,
          Total]),
        Log = io_lib:format("[~20s] - Server[~20w]:[  cancel]Task ~p finished {~w return ~w resource} but resource already release.~n"
          , [get_now_time(), ?MODULE, Key,Pid,Amount]),
        log(Log),
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
  ets:to_dets(Ets, ?MODULE),
  dets:close(?MODULE),
  Log = io_lib:format("[~20s] - Server[~20w]:[sys_info]Server Offline.~n~n~n", [get_now_time(), ?MODULE]),
  log(Log),
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
  Log = io_lib:format("[~20s] - Server[~20w]:[   start]Task ~p start {~w apply ~w resource ~w ms}.~n",
    [get_now_time(), ?MODULE, Key,Pid,Amount,Time]),
  log(Log),
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
          dets:to_ets(?MODULE, Ets);
        false ->
          ok = dets:insert(?MODULE, {0, init})
      end;
    {error, Reason} ->
      error(Reason)
  end,
  dets:close(?MODULE),
  Ets.

get_now_time() ->
  {{Year, Month, Day}, {Hour, Minute, Second}} = calendar:now_to_datetime(erlang:timestamp()),
  Time = integer_to_list(Year) ++ "/" ++ integer_to_list(Month) ++ "/" ++ integer_to_list(Day)
    ++ "  " ++ integer_to_list(Hour) ++ ":" ++ integer_to_list(Minute) ++ ":" ++ integer_to_list(Second),
  list_to_bitstring(Time).

log(Info) ->
  ok = file:write_file("./logs/log.txt",Info,[append]).

%%%-------------------------------------------------------------------
%%% @author ericwong
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 08. Feb 2021 23:46
%%%-------------------------------------------------------------------
-module(ets_resource_client).
-author("ericwong").

-behaviour(gen_server).

-import(ets_resource_server, [request_resource/2, return_resource/1]).

%% API
-export([start_link/0, start/0, resource_already_release/2]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).

-define(INTERVAL, 1000).          %% 客户端请求间隔

-define(MAX_HOLD_TIME, 600).      %% 客户端占有资源时间
                                  %% 要小于客户端请求间隔
                                  %% 以确保同一时刻客户端持有一组资源

-define(MAX_REQUEST_AMOUNT, 20).  %% 客户端一次申请资源最大值
                                  %% 要小于服务端总体资源数量

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
start() ->
  gen_server:start(?MODULE, [], []).
resource_already_release(Name, Key) ->
  gen_server:call(Name, {release_resource, Key}).

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
  Name = random_num(0, 99999),
  register(list_to_atom(integer_to_list(Name)), self()),
  io:format("Cilent[~20w]:online~n", [Name]),
  erlang:send_after(rand:uniform(500), self(), {request_base_resource_server, Name}),
  {ok, -1}.

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

handle_call({release_resource, Key}, _From, State) ->
  NewState =
    if
      Key =:= State -> -1;
      true -> -1  %% 失败证明服务端已经回收资源 客户端相应的重置Key
    end,
  {reply, ok, NewState}.

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
handle_info({request_base_resource_server, Name}, State) ->
  Amount = random_num(1, ?MAX_REQUEST_AMOUNT),
  Time = random_num(0, ?MAX_HOLD_TIME),
  Return_Time = random_num(0, ?INTERVAL),
  io:format("Cilent[~20w]:{Amount,~p},{Time,~p}~n", [Name, Amount, Time]),
  NewState =
    case request_resource(Amount, Time) of
      {ok, Key} when Return_Time < Time ->
        erlang:send_after(Return_Time, self(), {return_resource, Name}),
        Key;
      {ok, Key} -> Key;
      _ -> State
    end,
  erlang:send_after(?INTERVAL, self(), {request_base_resource_server, Name}),
  {noreply, NewState};

handle_info({return_resource, Name}, State) ->
  io:format("Cilent[~20w]:{~p},{Key,~p}~n", [Name, return_resource, State]),
  NewState =
    case return_resource(State) of
      {ok, return_succeed} -> -1;
      error -> -1  %% 失败证明服务端已经回收资源 客户端相应的重置Key
    end,
  {noreply, NewState}.

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
random_num(From, To) ->
  rand:uniform(To - From) + From.
%%==============================================================================%%
%% @author SaveMos
%% @copyright (C) 2025, <FleetFra>
%% @doc
%% WebSocket Manager module for handling WebSocket processes storage using ETS.
%% This module provides API functions for storing, retrieving, and deleting WebSocket processes.
%% It also includes periodic cleanup of expired WebSocket processes.
%% @end
%% Created : 29. Jan 2025 15:03
%%==============================================================================%%
-module(websocket_manager).
-author("Saverio").

%% API
-export([start_link/0, store_pid/2, get_opponent_pid/3, remove_pid/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, terminate/2, code_change/3]).

%% Define ETS table name
-define(ETS_TABLE, websocket_registry).

%%%===================================================================
%%% API functions
%%%===================================================================

%% @doc Starts the websocket manager.
start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @doc Stores the PID of a WebSocket process associated with a GameID and PlayerID.
store_pid(GameID, PlayerID) ->
  io:format("Memorizing PID for GameID: ~p, PlayerID: ~p~n", [GameID, PlayerID]),
  gen_server:cast(?MODULE, {store_pid, GameID, PlayerID, self()}).

%% @doc Retrieves the PID of the opponent player.
get_opponent_pid(GameID, PlayerID, WaitingPlayer) ->
  gen_server:call(?MODULE, {get_opponent_pid, GameID, PlayerID, WaitingPlayer}).

%% @doc Removes a PID from the registry when a connection is closed.
remove_pid(GameID, PlayerID) ->
  gen_server:cast(?MODULE, {remove_pid, GameID, PlayerID}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
  ets:new(?ETS_TABLE, [set, public, named_table]),
  {ok, #{}}.

handle_call({get_opponent_pid, GameID, PlayerID , OpponentID}, _From, State) ->
  PlayerID_Str = utility:to_str(PlayerID),
  GameID_str = utility:to_str(GameID),
  io:format("Searching for opponent PID for GameID: ~p, PlayerID: ~p~n", [GameID_str, PlayerID_Str]),
  case ets:lookup(?ETS_TABLE, {GameID_str, PlayerID_Str}) of
    [{_, PID}] ->
      OpponentID_Str = utility:to_str(OpponentID),
      io:format("Looking for opponent: ~p~n", [OpponentID_Str]),
      case ets:lookup(?ETS_TABLE, {GameID_str, OpponentID_Str}) of
        [{_, OpponentPID}] ->
          io:format("Found: ~p~n", [OpponentID]),
          {reply, {ok, OpponentPID}, State};
        [] -> {reply, {error, not_found}, State}
      end;
    [] ->
      {reply, {error, not_found}, State}
  end.


handle_cast({store_pid, GameID, PlayerID, PID}, State) ->
  ets:insert(?ETS_TABLE, {{GameID, PlayerID}, PID}),
  {noreply, State};
handle_cast({remove_pid, GameID, PlayerID}, State) ->
  ets:delete(?ETS_TABLE, {GameID, PlayerID}),
  {noreply, State}.

terminate(_Reason, _State) ->
  ets:delete(?ETS_TABLE),
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

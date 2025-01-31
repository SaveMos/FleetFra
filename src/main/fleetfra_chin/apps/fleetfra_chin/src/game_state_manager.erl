%%%-------------------------------------------------------------------
%% @author SaveMos
%% @copyright (C) 2025, <FleetFra>
%%% @doc
%%% Game State Manager module for handling game state storage using ETS.
%%% This module provides API functions for storing, retrieving, and deleting game states.
%%% It also includes periodic cleanup of expired game states.
%%% @end
%%% Created : 29. Jan 2025 15:03
%%%-------------------------------------------------------------------
-module(game_state_manager).
-author("SaveMos").
-behaviour(gen_server).

%% API
-export([start_link/0, put_game_state/2, get_game_state/1, delete_game_state/1]).

%% GenServer Callbacks
-export([init/1, handle_call/3, handle_cast/2 ,handle_info/2]).

-record(game, {game_id, players, battlefields, current_turn, game_over, winner, created_at}).

%%% =========================
%%% API for ETS Management
%%% =========================

%% Starts the GameStateManager process
start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% Inserts a new game state into ETS
put_game_state(GameID, GameState) ->
  gen_server:call(?MODULE, {put, GameID, GameState}).

%% Retrieves a game state from ETS
get_game_state(GameID) ->
  gen_server:call(?MODULE, {get, GameID}).

%% Deletes a game state from ETS
delete_game_state(GameID) ->
  gen_server:call(?MODULE, {delete, GameID}).

%%% =========================
%%% GenServer Callbacks
%%% =========================

init([]) ->
  ets:new(game_state_table, [named_table, public, set]),
  io:format("SYSTEM: GameState ETS initialized.~n"),

  % Start the periodic cleanup process every hour
  erlang:send_after(fleetfra_chin_configuration:get_auto_clean_period(), self(), clean_old_games),
  io:format("SYSTEM: Auto-clean system initialized.~n"),
  {ok, #{}}.

%% Unique definition of handle_call/3
handle_call({put, GameID, GameState}, _From, State) ->
  % Handles insertion of a new game state into ETS.
  ets:insert(game_state_table, {GameID, GameState}),
  {reply, ok, State};

handle_call({get, GameID}, _From, State) ->
  % Handles retrieval of a game state associated with a GameID.
  case ets:lookup(game_state_table, GameID) of
    [{GameID, GameState}] -> {reply, {ok, GameState}, State};
    [] -> {reply, {error, not_found}, State}
  end;

handle_call({delete, GameID}, _From, State) ->
  % Handles removal of a game state from ETS.
  ets:delete(game_state_table, GameID),
  {reply, ok, State};
handle_call({update, GameID, NewGameState}, _From, State) ->
  case ets:lookup(game_state_table, GameID) of
    [{GameID, _OldGameState}] ->
      ets:insert(game_state_table, {GameID, NewGameState}), % Overwrites the existing state
      {reply, ok, State};
    [] ->
      {reply, {error, not_found}, State}
  end.

handle_cast({push, Item}, State) ->
  %% Adds an item to the queue (list) and returns the new state
  {noreply, [Item | State]}; %% No response sent

handle_cast({pop}, [Head | Tail]) ->
  %% Removes the top item from the queue
  io:format("Popped item: ~p~n", [Head]),
  {noreply, Tail}; %% Returns the updated queue
handle_cast({pop}, []) ->
  %% If the queue is empty, do nothing
  io:format("Queue is empty~n"),
  {noreply, []}.

handle_info(clean_old_games, State) ->
  Now = erlang:system_time(second),
  ExpirationTime = fleetfra_chin_configuration:get_max_match_age(),  % 20 seconds for testing

  % Retrieve all game states from ETS
  Games = ets:tab2list(game_state_table),

  lists:foreach(fun({GameID, GameState}) ->
    CreatedAt = GameState#game.created_at,
    case Now - CreatedAt >= ExpirationTime of
      true ->
        io:format("Deleting expired game: ~p~n", [GameID]),
        ets:delete(game_state_table, GameID);
      false -> ok
    end
                end, Games),

% Schedule the next cleanup execution after 1 second
  erlang:send_after(fleetfra_chin_configuration:get_auto_clean_period(), self(), clean_old_games),

  {noreply, State};

handle_info(_Msg, State) ->
  io:format("Received unexpected message: ~p~n", [_Msg]),
  {noreply, State}.

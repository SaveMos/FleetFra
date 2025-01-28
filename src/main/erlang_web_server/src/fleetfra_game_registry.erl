%%%-------------------------------------------------------------------
%%% @author Saverio
%%% @copyright (C) 2025, <FleetFra>
%%% @doc
%%%
%%% This module handles the game registry, including generating unique
%%% game IDs and starting new games. It also manages looking up game processes.
%%%
%%% @end
%%% Created : 28. gen 2025 14:00
%%%-------------------------------------------------------------------
-module(fleetfra_game_registry).
-author("Saverio").

%% Exported functions
-export([generate_game_id/2, start_new_game/1, get_game_process/1]).

%%% Function to generate a unique game ID
generate_game_id(Player1, Player2) ->
  %% Get current timestamp in milliseconds
  Timestamp = erlang:system_time(milliseconds),

  %% Sort player names alphabetically and concatenate with underscore
  [First, Second] = lists:sort([Player1, Player2]),

  %% Concatenate player names and timestamp to form a unique game ID
  lists:concat([First, "_", Second, "_", integer_to_list(Timestamp)]).

%%% Function to start a new game
start_new_game(Json) ->
  %% Extract player names from the JSON
  Player1 = maps:get(<<"player1">>, Json),
  Player2 = maps:get(<<"player2">>, Json),

  %% Generate a unique game ID
  GameId = generate_game_id(Player1, Player2),

  %% Check if a game with this ID is already running
  case whereis({game_process, GameId}) of
    undefined ->
      %% Spawn a new process for the game
      Pid = fleetfra_game_process:start_game(GameId, Player1, Player2),
      %% Register the game process by its unique ID
      register({game_process, GameId}, Pid),
      io:format("Game ~s started and registered with process {game_process, ~s} (PID: ~p)~n", [GameId, GameId, Pid]),
      {ok, GameId};
    _ ->
      io:format("Error: Game with ID ~s is already running!~n", [GameId]),
      {error, already_exists}
  end.

%%% Function to get the process of a game by its ID
get_game_process(GameId) ->
  case whereis({game_process, GameId}) of
    undefined -> {error, not_found};
    GameProcess -> {ok, GameProcess}
  end.

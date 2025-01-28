%%%-------------------------------------------------------------------
%%% @author Saverio
%%% @copyright (C) 2025, <FleetFra>
%%% @doc
%%%
%%% This module handles the game logic for a single game process.
%%% It listens for messages such as `{move, MoveData}` to process player moves.
%%%
%%% @end
%%% Created : 28. gen 2025 14:00
%%%-------------------------------------------------------------------
-module(fleetfra_game_process).
-author("Saverio").

%% Internal functions
-export([start_game/3, handle_move/2, game_process/3]).

%%% Starting the game process
start_game(GameId, Player1, Player2) ->
  %% Create a new game process for the given GameId and players
  spawn(fun() -> game_process(GameId, Player1, Player2) end).

%%% Main game process loop
game_process(GameId, Player1, Player2) ->
  %% Initial game state (could be expanded with more fields)
  State = #{game_id => GameId, player1 => Player1, player2 => Player2, moves => []},

  %% Enter loop to listen for game messages
  loop(State).

%%% Loop function to listen for game-related messages
loop(State) ->
  receive
  %% Message to process a move
    {move, MoveData} ->
      %% Handle the move
      NewState = handle_move(MoveData, State),
      %% Continue waiting for more messages with updated state
      loop(NewState);

  %% Unexpected messages (this case is handled generically)
    _OtherMessage ->
      io:format("Unexpected message received in game ~s~n", [State#game_id]),
      loop(State)
  end.

%%% Function to handle a move message
handle_move(MoveData, State) ->
  %% Here we would process the move, update the game state, and check if the game is over
  %% For now, just log the move data
  io:format("Processing move: ~p~n", [MoveData]),

  %% Update the moves in the game state (just adding the move for now)
  NewMoves = State:moves ++ [MoveData],

  %% Update the state (you would also add logic to check for game completion or validation of moves)
  State1 = State#{moves => NewMoves},

  %% Return the updated state
  State1.

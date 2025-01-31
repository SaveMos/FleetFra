-module(fleetfra_game).
-export([start_game/2, make_move/2]).
-author("SaveMos").
%%-------------------------------------------------------------------
%% @author SaveMos
%% @copyright (C) 2025, <FleetFra>
%% @doc
%% Starts a new game by storing the initial game state and the players' battlefields.
%% The game state includes the game ID, the players' names, their battlefields, the current turn, and whether the game is over.
%% @param GameID The unique identifier for the game.
%% @param {Player1, Player2, Battlefield1, Battlefield2} A tuple containing the players' names and their battlefields.
%% @return void
%%-------------------------------------------------------------------
-record(game, {game_id, players, battlefields, current_turn, game_over, winner, created_at}).

start_game(GameID, {Player1, Player2, Battlefield1, Battlefield2}) ->
  %% Initialize ETS table
  init_ets(),

  %% Create the initial game state using the correct map syntax
  GameState = #game{
    game_id = GameID,
    players = #{player1 => Player1, player2 => Player2},
    battlefields = #{player1 => Battlefield1, player2 => Battlefield2},
    current_turn = player1,
    game_over = false,
    winner = none,
    created_at = erlang:system_time(second)  % Timestamp of creation in seconds.
  },

  % Store the game state in the ETS.
  game_state_manager:put_game_state(GameID, GameState),
  {ok, GameState}.

%% Initialize ETS table before inserting game states
init_ets() ->
  case ets:info(game_state_table) of
    undefined -> ets:new(game_state_table, [named_table, public, set]);
    _ -> ok
  end.

%%-------------------------------------------------------------------
%% @author SaveMos
%% @copyright (C) 2025, <FleetFra>
%% @doc
%% Makes a move in the game by updating the corresponding battlefield.
%% It checks if the move is valid and updates the game state accordingly.
%% @param GameID The unique identifier for the game.
%% @param {Player, {Row, Col}} A tuple containing the player's name and the coordinates of the move.
%% @return {ok, NewGameState} if the move is valid and accepted, or an error tuple if invalid.
%%-------------------------------------------------------------------
make_move(GameID, {Player, {Row, Col}}) ->
  PlayerAtom = binary_to_atom(Player, utf8),
  io:format("#######################################################################################~n"),
  io:format("~p ~n" , [PlayerAtom]),
  io:format("#######################################################################################~n"),

  case game_state_manager:get_game_state(GameID) of
    {ok, GameState} ->
      case GameState#game.current_turn of
        PlayerAtom ->
          io:format("OK: ~p can play!~n" , [PlayerAtom]),

          case maps:find(PlayerAtom, GameState#game.battlefields) of
            {ok, PlayerBattlefield} ->
              io:format("OK: ~p battlefield found!~n"  , [PlayerAtom]),
              case GameState#game.game_over of
                true ->
                  case GameState#game.winner of
                    PlayerAtom -> {fin, winner};
                    _ -> game_state_manager:delete_game_state(GameID),
                      {fin, loser}
                  end;
                false ->
                  %% Update the battlefield with the move
                  UpdatedBattlefield = update_battlefield(PlayerBattlefield, Row, Col),

                  %% Use update_game_state to update the GameState
                  NewGameState = update_game_state(GameState, Player, UpdatedBattlefield),

                  %% Save the new game state
                  game_state_manager:put_game_state(GameID, NewGameState),

                  io:format("REPORT: Match state [~p] -> # ~p # ~n", [NewGameState#game.game_over , NewGameState#game.winner]),

                  case NewGameState#game.game_over of
                    true ->
                      case NewGameState#game.winner of
                        PlayerAtom -> {fin, winner};
                        _ -> game_state_manager:delete_game_state(GameID),
                          {fin, loser}
                      end;
                    _ -> {ok, ok_move}
                  end
              end;

            error ->
              io:format("ERROR: ~p battlefield not found!~n"  , [PlayerAtom]),
              {error, player_not_found}
          end;

        _ ->
          io:format("ERROR: ~p can't play yet!~n" , [PlayerAtom]),
          {error, not_your_turn} %% It's not their turn.
      end;

    {error, not_found} ->
      io:format("ERROR: Can't find the match ~p~n", [GameID]),
      {error, game_not_found}
  end.

%%-------------------------------------------------------------------
%% @author SaveMos
%% @copyright (C) 2025, <FleetFra>
%% @doc
%% Updates the battlefield matrix with a new value at the specified row and column.
%% This function assumes the battlefield is a 10x10 matrix represented as a list of lists.
%% @param Battlefield The current battlefield.
%% @param Row The row index of the cell to update.
%% @param Col The column index of the cell to update.
%% @param NewValue The new value to set at the specified cell.
%% @return The updated battlefield.
%%-------------------------------------------------------------------

update_battlefield(Battlefield, Row, Col) ->
  [case Cell of
     #{<<"row">> := RowIndex, <<"col">> := ColIndex} when RowIndex == Row andalso ColIndex == Col ->
       %% Determine the new value for the cell
       NewValue = case Cell of
                    #{<<"value">> := 0} -> 3;  %% If water is hit (0 -> Untouched water)
                    #{<<"value">> := 1} -> 2;  %% If the ship is not hit (1 -> Ship not hit)
                    #{<<"value">> := 2} -> 2;  %% If the ship is hit (2 -> Ship hit, remains unchanged)
                    #{<<"value">> := 3} -> 3;  %% If water is already hit (3 -> Water hit, remains unchanged)
                    _ -> 0  %% Any other unexpected value is untouched water
                  end,
       %% Check if the cell is adjacent to a hit ship (-1 for adjacent water)
       case check_adjacent_to_shot(Battlefield, Row, Col) of
         true -> Cell#{<<"value">> => -1};  %% If the cell is adjacent to a hit ship, set it to -1
         false -> Cell#{<<"value">> => NewValue}  %% Otherwise, set the value as determined
       end;

     _ -> Cell  %% Leave other cells unchanged
   end || Cell <- Battlefield].

%% Function that checks if a cell is adjacent to a hit ship
check_adjacent_to_shot(Battlefield, Row, Col) ->
  %% Checks the adjacent cells (diagonal, horizontal, vertical)
  lists:any(fun(Cell) ->
    case Cell of
      #{<<"row">> := R, <<"col">> := C, <<"value">> := 2} when abs(R - Row) =< 1 andalso abs(C - Col) =< 1 -> true;
      _ -> false
    end
            end, Battlefield).

%%-------------------------------------------------------------------
%% @author SaveMos
%% @copyright (C) 2025, <FleetFra>
%% @doc
%% Updates the game state after a move by updating the battlefield, the current turn, and checking if the game is over.
%% @param GameState The current game state.
%% @param Player The player who made the move.
%% @param NewBattlefield The updated battlefield of the player.
%% @return The updated game state.
%%-------------------------------------------------------------------
update_game_state(GameState, Player, NewBattlefield) ->
  %% Convert Player to an atom if it's a binary
  PlayerAtom = case is_binary(Player) of
                 true -> binary_to_atom(Player, utf8);
                 false -> Player
               end,

  %% Update the battlefields map with the correct key (PlayerAtom)
  NewBattlefields = maps:put(PlayerAtom, NewBattlefield, GameState#game.battlefields),

  %% Switch the turn
  NewTurn = case PlayerAtom of
              player1 -> player2;
              _ -> player1
            end,

  %% Get the current player's battlefield
  PlayerBattlefield = maps:get(PlayerAtom, NewBattlefields),

  %% Check if the game is over
  GameOver = check_game_over(PlayerBattlefield),

  %% Determine the winner if the game is over
  Winner = case GameOver of
             true -> PlayerAtom;
             _ -> none
           end,

  %% Return the updated GameState
  GameState#game{battlefields = NewBattlefields, current_turn = NewTurn, game_over = GameOver, winner = Winner, created_at = erlang:system_time(second)}.

%%-------------------------------------------------------------------
%% @author SaveMos
%% @copyright (C) 2025, <FleetFra>
%% @doc
%% Checks if the game is over, meaning all the ships of one player have been hit.
%% @param GameState The current game state.
%% @return true if the game is over, false otherwise.
%%-------------------------------------------------------------------

check_game_over(Battlefield) ->
  case lists:member(true, [case Cell of
                             #{<<"value">> := 1} -> true;  %% If the cell is an unhit ship
                             _ -> false
                           end || Cell <- Battlefield]) of
    true -> false;  %% If there's at least one unhit ship, the game is not over
    false -> true   %% If all ships are hit, the game is over
  end.

%% Print the current game state
print_game_state(GameID) ->
  io:format("--------------------------------------------------------------------------------------------~n"),
  io:format("CURRENT GAME STATE~n"),
  io:format("--------------------------------------------------------------------------------------------~n"),
  case game_state_manager:get_game_state(GameID) of
    {ok, ThisGame} ->
      io:format("GameState: ~p~n", [ThisGame]),
      io:format("--------------------------------------------------------------------------------------------~n");
    {error, not_found} ->
      io:format("Error: Game not found~n"),
      io:format("--------------------------------------------------------------------------------------------~n")
  end.

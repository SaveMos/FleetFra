-module(fleetfra_game).
-export([start_game/2 ,start_game_client/2, make_move/2 , get_game_info/1]).
-author("SaveMos").
%%-------------------------------------------------------------------
%% @author SaveMos
%% @copyright (C) 2025, <FleetFra>
%% @doc
%% Starts a new game by storing the initial game state and the players' battlefields.
%% The game state includes the game ID, the players' names, their battlefields, the current turn, and whether the game is over.
%% @param GameID The unique identifier for the game.
%% @param {Player1, Player2, Battlefield1, Battlefield2} A tuple containing the players' names and their battlefields.
%% @end
%%-------------------------------------------------------------------
-record(game, {game_id, player1 , player2, battlefields, current_turn, waiting_player ,game_over, winner, created_at , init_complete}).

start_game(GameID, {Player1, Player2, Battlefield1, Battlefield2}) ->
  %% Initialize ETS table
  init_ets(),
  PlayerAtom1 = bin_to_atom(Player1),
  PlayerAtom2 = bin_to_atom(Player2),

  %% Create the initial game state using the correct map syntax
  GameState = #game{
    game_id = GameID,
    player1 = PlayerAtom1,
    player2 = PlayerAtom2,
    battlefields = #{PlayerAtom1 => Battlefield2, PlayerAtom2 => Battlefield1},
    current_turn = PlayerAtom1,
    waiting_player = PlayerAtom2,
    game_over = false,
    winner = none,
    created_at = erlang:system_time(second),  % Timestamp of creation in seconds.
    init_complete = true
  },

  % Store the game state in the ETS.
  game_state_manager:put_game_state(GameID, GameState),
  {ok, GameState}.

%%-------------------------------------------------------------------
%% @author SaveMos
%% @copyright (C) 2025, <FleetFra>
%% @doc
%% Starts a new game by storing the initial game state and the players' battlefields.
%% The starting procedure is composed by two steps.
%% The first step starts when one of the player send the "start_game_client" request.
%% In the first step the initial structure is created and partially filled.
%% The second step starts when the other player send the "start_game_client" request.
%% In the second step the  structure is filled with the remnants information.
%% The structure represents the game state and it includes the game ID, the players' names, their battlefields,
%% the current turn, and whether the game is over and other information.
%% @param GameID The unique identifier for the game.
%% @param {Player, Battlefield} A tuple containing the player names and his battlefield.
%% @end
%%-------------------------------------------------------------------

start_game_client(GameID, {Player, Battlefield}) ->
  init_ets(), %% Initialize the ETS table.
  PlayerAtom = bin_to_atom(Player), % The player that made this request.

  case game_state_manager:get_game_state(GameID) of
    {ok, GameState} ->
      % Phase 2 - The second player send the final information.
      NewBattlefields = maps:put(PlayerAtom, Battlefield, GameState#game.battlefields),

      Player1 = GameState#game.player1,
      Player2 = PlayerAtom,
      Battlefield1 = maps:get(Player1, NewBattlefields),
      Battlefield2 = maps:get(Player2, NewBattlefields),

      % Swap the battlefields.
      % Battlefield1 is the field where Player1 shoots.
      SwappedBattlefields = NewBattlefields#{
        Player1 => Battlefield2,
        Player2 => Battlefield1
      },

      NewGameState = GameState#game{
        battlefields = SwappedBattlefields,
        player2 = PlayerAtom,
        waiting_player = PlayerAtom,
        game_over = false,
        winner = none,
        created_at = erlang:system_time(second),
        init_complete = true
      },
      %io:format("Fase 2 OK. ~p ~n" , [PlayerAtom]),
      game_state_manager:put_game_state(GameID, NewGameState),  % Store the game state in the ETS.
      {ok, NewGameState};

    {error, not_found} ->
      % Phase 1 - The first player sends the initial information.
      GameState = #game{
        game_id = GameID,
        player1 = PlayerAtom,
        player2 = undefined,
        battlefields = #{PlayerAtom => Battlefield},
        current_turn = PlayerAtom,
        waiting_player = undefined,
        game_over = false,
        winner = none,
        created_at = erlang:system_time(second),
        init_complete = false
      },
      %io:format("Fase 1 OK. ~p ~n" , [PlayerAtom]),
      game_state_manager:put_game_state(GameID, GameState),  % Store the game state in the ETS.
      {ok, GameState}
  end.

%%-------------------------------------------------------------------
%% @author SaveMos
%% @copyright (C) 2025, <FleetFra>
%% @doc
%% Cast a binary object into an atom.
%% @end
%%-------------------------------------------------------------------
bin_to_atom(Bin) ->
  case is_binary(Bin) of
      true -> binary_to_atom(Bin, utf8);
      false -> Bin
  end.

%%-------------------------------------------------------------------
%% @author SaveMos
%% @copyright (C) 2025, <FleetFra>
%% @doc
%% Initialize the ETS, use it before inserting game states.
%% @end
%%-------------------------------------------------------------------
init_ets() ->
  case ets:info(game_state_table) of
    undefined -> ets:new(game_state_table, [named_table, public, set]);
    _ -> ok
  end.

%%-------------------------------------------------------------------
%% @author SaveMos
%% @copyright (C) 2025, <FleetFra>
%% @doc
%% Return a JSON with all the information about a certain game.
%% @param GameID The unique identifier for the game.
%% @end
%%-------------------------------------------------------------------

get_game_info(GameID) ->
  case game_state_manager:get_game_state(GameID) of
    {ok, GameState} ->
      JsonResponse = game_state_to_json(GameState),
      {ok, JsonResponse};
    {error, not_found} ->
      {error, game_not_found}
  end.

%%-------------------------------------------------------------------
%% @author SaveMos
%% @copyright (C) 2025, <FleetFra>
%% @doc
%% Return a JSON with all the information about a certain game.
%% @param GameState The game structure.
%% @end
%%-------------------------------------------------------------------

game_state_to_json(#game{
  game_id = GameID,
  player1 = Player1,
  player2 = Player2,
  battlefields = Battlefields,
  current_turn = CurrentTurn,
  waiting_player = WaitingPlayer,
  game_over = GameOver,
  winner = Winner,
  created_at = CreatedAt,
  init_complete = InitComplete
}) ->
  JsonMap = #{
    <<"game_id">> => GameID,
    <<"player1">> => Player1,
    <<"player2">> => Player2,
    <<"battlefields">> => battlefields_to_json(Battlefields),
    <<"current_turn">> => CurrentTurn,
    <<"waiting_player">> => WaitingPlayer,
    <<"game_over">> => GameOver,
    <<"winner">> => Winner,
    <<"created_at">> => CreatedAt,
    <<"init_complete">> => InitComplete
  },
  jsx:encode(JsonMap).

battlefields_to_json(Battlefields) ->
  maps:map(fun(_, BF) -> battlefield_to_json(BF) end, Battlefields).

battlefield_to_json(Battlefield) when is_map(Battlefield) ->
  maps:values(Battlefield);
battlefield_to_json(Battlefield) when is_list(Battlefield) ->
  [cell_to_json(Cell) || Cell <- Battlefield].

cell_to_json(#{<<"row">> := Row, <<"col">> := Col, <<"value">> := Value}) ->
  #{<<"row">> => Row, <<"col">> => Col, <<"value">> => Value}.


%%-------------------------------------------------------------------
%% @author SaveMos
%% @copyright (C) 2025, <FleetFra>
%% @doc
%% Makes a move in the game by updating the corresponding battlefield.
%% It checks if the move is valid and updates the game state accordingly.
%% @param GameID The unique identifier for the game.
%% @param {Player, {Row, Col}} A tuple containing the player's name and the coordinates of the move.
%% @return {ok, NewGameState} if the move is valid and accepted, or an error tuple if invalid.
%% @end
%%-------------------------------------------------------------------
make_move(GameID, {Player, {Row, Col}}) ->
  PlayerAtom = bin_to_atom(Player),
  %io:format("#######################################################################################~n"),
  %io:format("~p ~n" , [PlayerAtom]),
  %io:format("#######################################################################################~n"),

  case game_state_manager:get_game_state(GameID) of
    {ok, GameState} ->
      case GameState#game.current_turn of
        PlayerAtom ->
          %io:format("OK: ~p can play!~n" , [PlayerAtom]),
          case maps:find(PlayerAtom, GameState#game.battlefields) of
            {ok, PlayerBattlefield} ->
              %io:format("OK: ~p battlefield found!~n"  , [PlayerAtom]),
              case GameState#game.init_complete of
                true ->
                  case GameState#game.game_over of
                    true ->
                      % The game is already ended, so the other player won.
                      case GameState#game.winner of
                        PlayerAtom ->
                          {fin, winner};
                        _ -> game_state_manager:delete_game_state(GameID),
                          {fin, loser}
                      end;
                    false ->
                      % The game is not over yet.
                      case check_move_coordinates(Row, Col) of
                        % Input check.
                        {ok , _} ->
                          %% Update the battlefield with the move
                          UpdatedBattlefield = update_battlefield(PlayerBattlefield, Row, Col),

                          %% Use update_game_state to update the GameState
                          NewGameState = update_game_state(GameState, Player, UpdatedBattlefield),

                          %% Save the new game state
                          game_state_manager:put_game_state(GameID, NewGameState),

                          %io:format("REPORT: Match state [~p] -> # ~p # ~n", [NewGameState#game.game_over , NewGameState#game.winner]),

                          case NewGameState#game.game_over of
                            % The game is ended.
                            true ->
                              case NewGameState#game.winner of
                                PlayerAtom ->
                                  {fin, winner};
                                _ ->
                                  game_state_manager:delete_game_state(GameID),
                                  {fin, loser}
                              end;
                            _ ->
                              {ok, ok_move}
                          end;
                        {error , out_of_bounds} ->
                          {error, out_of_bound_coordinates};
                        {error, not_integer} ->
                          {error, not_integer}
                      end
                  end;
                false ->
                  {error , game_not_initiated}
              end;
              error ->
                %io:format("ERROR: ~p battlefield not found!~n"  , [PlayerAtom]),
                {error, player_not_found}
          end;
       _ ->
          case GameState#game.waiting_player of
            PlayerAtom ->
              %io:format("ERROR: ~p can't play yet!~n" , [PlayerAtom]),
              {error, not_your_turn}; %% It's not their turn.
            _ ->
              {error, player_not_found} % The player does not exists.
          end
      end;
    {error, not_found} ->
      %io:format("ERROR: Can't find the match ~p~n", [GameID]),
      {error, game_not_found}
  end.

%%%-------------------------------------------------------------------
%%% @doc
%%% Validates the move coordinates to ensure they are within the valid range.
%%% Additionally, it checks that Row and Col are integers.
%%% The battlefield dimension is retrieved dynamically using `get_battlefield_dimension/0`.
%%%
%%% @param Row The row index of the move (integer).
%%% @param Col The column index of the move (integer).
%%%
%%% @returns {ok, proceed} if the move is valid.
%%%          {error, invalid_input} if Row or Col is not an integer.
%%%          {error, out_of_bounds} if the move is outside the allowed range.
%%% @end
%%%-------------------------------------------------------------------

check_move_coordinates(Row, Col) ->
  Dim = fleetfra_chin_configuration:get_battlefield_dimension(),

  case {is_integer(Row), is_integer(Col)} of
    {false, _} -> {error, not_integer};  %% Row is not an integer.
    {_, false} -> {error, not_integer};  %% Col is not an integer.
    {true, true} ->
      if
        Row < 0 orelse Row >= Dim orelse Col < 0 orelse Col >= Dim ->
          %% If the row or column is outside the battlefield range, return an error.
          {error, out_of_bounds};
        true ->
          %% If the move is valid, return a success tuple.
          {ok, proceed}
      end
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

%%-------------------------------------------------------------------
%% @author SaveMos
%% @copyright (C) 2025, <FleetFra>
%% @doc
%% Function that checks if a cell is adjacent to a hit ship
%%-------------------------------------------------------------------
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
  PlayerAtom = bin_to_atom(Player),

  %% Update the battlefields map with the correct key (PlayerAtom)
  NewBattlefields = maps:put(PlayerAtom, NewBattlefield, GameState#game.battlefields),

  %% Switch the turn
  NewTurn = GameState#game.waiting_player,

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
  GameState#game{battlefields = NewBattlefields, current_turn = NewTurn , waiting_player = PlayerAtom, game_over = GameOver, winner = Winner, created_at = erlang:system_time(second)}.

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

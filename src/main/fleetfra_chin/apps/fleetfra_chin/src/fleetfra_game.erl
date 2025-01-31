-module(fleetfra_game).
-export([start_game/2, make_move/2]).

%%-------------------------------------------------------------------
%% @doc
%% Starts a new game by storing the initial game state and the players' battlefields.
%% The game state includes the game ID, the players' names, their battlefields, the current turn, and whether the game is over.
%% @param GameID The unique identifier for the game.
%% @param {Player1, Player2, Battlefield1, Battlefield2} A tuple containing the players' names and their battlefields.
%% @return void
%%-------------------------------------------------------------------
-record(game, {game_id, players, battlefields, current_turn, game_over, winner}).

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
    winner = none
  },

  %put_game_state(GameID, GameState). %% Store the game state in the ETS.
  game_state_manager:put_game_state(GameID, GameState),
  {ok, GameState}.

%% Initialize ETS table before inserting game states
init_ets() ->
  case ets:info(game_state_table) of
    undefined -> ets:new(game_state_table, [named_table, public, set]);
    _ -> ok
  end.

%%-------------------------------------------------------------------
%% @doc
%% Makes a move in the game by updating the corresponding battlefield.
%% It checks if the move is valid and updates the game state accordingly.
%% @param GameID The unique identifier for the game.
%% @param {Player, {Row, Col}} A tuple containing the player's name and the coordinates of the move.
%% @return {ok, NewGameState} if the move is valid and accepted, or an error tuple if invalid.
%%-------------------------------------------------------------------
make_move(GameID, {Player, {Row, Col}}) ->
  %io:format("#######################################################################################~n"),
  %io:format("Move received: ~p~n", [{GameID, Player, Row, Col}]),
  %print_game_state(GameID),
  PlayerAtom = binary_to_atom(Player, utf8),

  case game_state_manager:get_game_state(GameID) of
    {ok, GameState} ->
      case GameState#game.current_turn of
        PlayerAtom ->
          io:format("CONFRONTO SUPERATO~n"),

          % Converte Player in atomo se è un binary

          case maps:find(PlayerAtom, GameState#game.battlefields) of
            {ok, PlayerBattlefield} ->
              case GameState#game.game_over of
                true ->
                  case GameState#game.winner of
                    PlayerAtom ->
                      {fin, winner};
                    _ ->
                      {fin, loser}
                  end;
                false ->
                  % Aggiorna il battlefield con la mossa
                  UpdatedBattlefield = update_battlefield(PlayerBattlefield, Row, Col),

                  % Usa update_game_state per aggiornare il GameState
                  NewGameState = update_game_state(GameState, Player, UpdatedBattlefield),

                  % Salva il nuovo stato del gioco
                  game_state_manager:put_game_state(GameID, NewGameState),

                  io:format("Stato Vittoria [~p] -> # ~p # ~n", [NewGameState#game.game_over , NewGameState#game.winner]),

                  case NewGameState#game.game_over of
                    true ->
                      case NewGameState#game.winner of
                        PlayerAtom ->
                          {fin, winner};
                        _ ->
                          {fin, loser}
                      end;
                    false->
                      {ok, ok_move}
                  end
              end;
              %io:format("Player battlefield found~n"),

            error ->
              io:format("Player battlefield not found~n"),
              {error, player_not_found}
          end;

        _ ->
          io:format("CONFRONTO FALLITO~n"),
          {error, not_your_turn} % Non è il suo turno.
      end;

    {error, not_found} ->
      io:format("PARTITA NON TROVATA~n"),
      {error, game_not_found}
  end.


%%-------------------------------------------------------------------
%% @doc
%% Updates the battlefield matrix with a new value at the specified row and column.
%% This function assumes the battlefield is a 10x10 matrix represented as a list of lists.
%% @param Battlefield The current battlefield.
%% @param Row The row index of the cell to update.
%% @param Col The column index of the cell to update.
%% @param NewValue The new value to set at the specified cell.
%% @return The updated battlefield.
%%-------------------------------------------------------------------

%% Funzione per aggiornare il battlefield
update_battlefield(Battlefield, Row, Col) ->
  [case Cell of
     #{<<"row">> := RowIndex, <<"col">> := ColIndex} when RowIndex == Row andalso ColIndex == Col ->
       % Determina il nuovo valore della cella
       NewValue = case Cell of
                    #{<<"value">> := 0} -> 3;  % Se l'acqua è colpita (0 -> Acqua non colpita)
                    #{<<"value">> := 1} -> 2;  % Se la nave non è colpita (1 -> Nave non colpita)
                    #{<<"value">> := 2} -> 2;  % Se la nave è colpita (2 -> Nave colpita, rimane invariato)
                    #{<<"value">> := 3} -> 3;  % Se l'acqua è già colpita (3 -> Acqua colpita, rimane invariato)
                    _ -> 0  % Qualsiasi altro valore non previsto è acqua non colpita
                  end,
       % Verifica se la cella è adiacente a una nave colpita (-1 per acqua vicina)
       case check_adjacent_to_shot(Battlefield, Row, Col) of
         true -> Cell#{<<"value">> => -1};  % Se la cella è vicina a una nave colpita, metti -1
         false -> Cell#{<<"value">> => NewValue}  % Altrimenti, imposta il valore come determinato
       end;

     _ -> Cell  % Lascia inalterate le altre celle
   end || Cell <- Battlefield].

% Funzione che verifica se una cella è adiacente a una cella di nave colpita
check_adjacent_to_shot(Battlefield, Row, Col) ->
  % Controlla le celle adiacenti (diagonali, orizzontali, verticali)
  lists:any(fun(Cell) ->
    case Cell of
      #{<<"row">> := R, <<"col">> := C, <<"value">> := 2} when abs(R - Row) =< 1 andalso abs(C - Col) =< 1 -> true;
      _ -> false
    end
            end, Battlefield).



%%-------------------------------------------------------------------
%% @doc
%% Updates the game state after a move by updating the battlefield, the current turn, and checking if the game is over.
%% @param GameState The current game state.
%% @param Player The player who made the move.
%% @param NewBattlefield The updated battlefield of the player.
%% @return The updated game state.
%%-------------------------------------------------------------------
update_game_state(GameState, Player, NewBattlefield) ->
  % Converti Player in atomo se è un binary
  PlayerAtom = case is_binary(Player) of
                 true -> binary_to_atom(Player, utf8);
                 false -> Player
               end,

  % Aggiorna il campo battlefields con la chiave corretta (PlayerAtom)
  NewBattlefields = maps:put(PlayerAtom, NewBattlefield, GameState#game.battlefields),

  % Alterna il turno
  NewTurn = case PlayerAtom of
              player1 -> player2;
              _ -> player1
            end,

  % Recupera il battlefield del giocatore corrente
  PlayerBattlefield = maps:get(PlayerAtom, NewBattlefields),

  % Verifica se il gioco è finito
  GameOver = check_game_over(PlayerBattlefield),

  io:format("GameOver ~p ~n", [GameOver]),

  Winner = case GameOver of
              true -> PlayerAtom;
              _ -> none
            end,

  % Restituisci il nuovo GameState
  GameState#game{battlefields = NewBattlefields, current_turn = NewTurn, game_over = GameOver, winner = Winner}.


%%-------------------------------------------------------------------
%% @doc
%% Checks if the game is over, meaning all the ships of one player have been hit.
%% @param GameState The current game state.
%% @return true if the game is over, false otherwise.
%%-------------------------------------------------------------------

check_game_over(Battlefield) ->
  case lists:member(true, [case Cell of
                             #{<<"value">> := 1} -> true;  % Se la cella è una nave non colpita
                             _ -> false
                           end || Cell <- Battlefield]) of
    true -> false;  % Se troviamo almeno una nave non colpita, il gioco non è finito
    false -> true   % Se tutte le navi sono colpite, il gioco è finito
  end.


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


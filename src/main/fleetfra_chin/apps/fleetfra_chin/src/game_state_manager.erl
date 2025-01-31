%%%-------------------------------------------------------------------
%%% @author Saverio
%%% @copyright (C) 2025, <FleetFra>
%%% @doc
%%%
%%% @end
%%% Created : 29. gen 2025 15:03
%%%-------------------------------------------------------------------
-module(game_state_manager).
-author("Saverio").
-behaviour(gen_server).

%% API
-export([start_link/0, put_game_state/2, get_game_state/1, delete_game_state/1]).

%% GenServer Callbacks
-export([init/1, handle_call/3, handle_cast/2 ,handle_info/2]).

-record(game, {game_id, players, battlefields, current_turn, game_over, winner, created_at}).

%%% =========================
%%% API per gestire ETS
%%% =========================

%% Avvia il processo del GameStateManager
start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% Inserisce un nuovo stato di gioco nella ETS
put_game_state(GameID, GameState) ->
  gen_server:call(?MODULE, {put, GameID, GameState}).


%% Recupera uno stato di gioco dalla ETS
get_game_state(GameID) ->
  gen_server:call(?MODULE, {get, GameID}).

%% Cancella un gioco dalla ETS
delete_game_state(GameID) ->
  gen_server:call(?MODULE, {delete, GameID}).


%%% =========================
%%% GenServer Callbacks
%%% =========================

init([]) ->
  ets:new(game_state_table, [named_table, public, set]),
  io:format("SYSTEM: GameState ETS initialized.~n"),

  % Avvia il processo periodico di pulizia ogni ora
  erlang:send_after(fleetfra_chin_configuration:get_auto_clean_period(), self(), clean_old_games),
  io:format("SYSTEM: Auto-clean system initialized.~n"),
  {ok, #{}}.


%% Definizione unica di handle_call/3
handle_call({put, GameID, GameState}, _From, State) ->
  % Gestisce il caso in cui un nuovo stato di gioco deve essere inserito nell'ETS.
  ets:insert(game_state_table, {GameID, GameState}),
  {reply, ok, State};

handle_call({get, GameID}, _From, State) ->
  % Gestisce la richiesta di recuperare lo stato di gioco associato a un GameID.
  case ets:lookup(game_state_table, GameID) of
    [{GameID, GameState}] -> {reply, {ok, GameState}, State};
    [] -> {reply, {error, not_found}, State}
  end;

handle_call({delete, GameID}, _From, State) ->
  % Gestisce la richiesta di rimuovere lo stato di gioco associato a un GameID da ETS.
  ets:delete(game_state_table, GameID),
  {reply, ok, State};
handle_call({update, GameID, NewGameState}, _From, State) ->
  case ets:lookup(game_state_table, GameID) of
    [{GameID, _OldGameState}] ->
      ets:insert(game_state_table, {GameID, NewGameState}), % Sovrascrive lo stato
      {reply, ok, State};
    [] ->
      {reply, {error, not_found}, State}
  end.


handle_cast({push, Item}, State) ->
  %% Aggiungi un elemento alla coda (lista) e restituisci il nuovo stato
  {noreply, [Item | State]}; %% Non inviamo una risposta

handle_cast({pop}, [Head | Tail]) ->
  %% Rimuovi l'elemento in cima alla coda
  io:format("Popped item: ~p~n", [Head]),
  {noreply, Tail}; %% Restituiamo la coda aggiornata
handle_cast({pop}, []) ->
  %% Se la coda è vuota, non fare nulla
  io:format("Queue is empty~n"),
  {noreply, []}.

handle_info(clean_old_games, State) ->
  Now = erlang:system_time(second),
  ExpirationTime = fleetfra_chin_configuration:get_max_match_age(),  % 20 secondi per il test

  % Recupera tutti i game states dalla ETS
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


% Pianifica la prossima esecuzione dopo 1 secondo
  erlang:send_after(fleetfra_chin_configuration:get_auto_clean_period(), self(), clean_old_games),

  {noreply, State};

handle_info(_Msg, State) ->
  io:format("Received unexpected message: ~p~n", [_Msg]),
  {noreply, State}.

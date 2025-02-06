-module(fleetfra_chin_ws_handler).
-behaviour(cowboy_websocket).
-author("SaveMos").
-export([
  init/2,
  websocket_init/1,
  websocket_handle/2,
  websocket_info/3,
  websocket_info/2,
  terminate/3
]).

%%==============================================================================%%
%% @author SaveMos
%% @copyright (C) 2025, <FleetFra>
%% @doc
%% WebSocket handler for FleetFra Chin.
%% This module manages WebSocket connections, processing incoming messages
%% and delegating request handling to `fleetfra_chin_handler`.
%% @end
%%==============================================================================%%

%%------------------------------------------------------------------------------
%% @author SaveMos
%% @copyright (C) 2025, <FleetFra>
%% @doc
%% Called when the WebSocket connection is initialized.
%% Stores the process PID.
%% @param State The state of the WebSocket connection.
%% @return A tuple indicating success and returning the state.
%% @end
%%------------------------------------------------------------------------------

init(Req, State) ->
  case extract_params(Req) of
    {GameID, PlayerID} ->
      websocket_manager:store_pid(GameID, PlayerID);
    {error, _} ->
      pass
  end,
  {cowboy_websocket, Req, State}.

%%------------------------------------------------------------------------------
%% @author SaveMos
%% @copyright (C) 2025, <FleetFra>
%% @doc Called when the WebSocket connection is initialized.
%% @param State The state of the WebSocket connection.
%% @return A tuple indicating success and returning the state.
%%------------------------------------------------------------------------------
websocket_init(State) ->
  {ok, State}.

%%------------------------------------------------------------------------------
%% @author SaveMos
%% @copyright (C) 2025, <FleetFra>
%% @doc
%% Helper function to extract parameters from WebSocket request
%% Extracts the GameID and the PlayerID from the WebSocket request URI.
%% @param State The state of the WebSocket connection.
%% @return A tuple indicating success and returning the state.
%% @end
%%------------------------------------------------------------------------------

extract_params(Req) ->
  % Estrai la query string
  QueryString = cowboy_req:qs(Req),

  % Converti la query string da binary a lista di caratteri (stringa)
  QueryStringStr = binary:bin_to_list(QueryString),

  % Estrai i parametri dalla query string e ritorna GameID e PlayerID
  parse_query_string(QueryStringStr).

% Funzione per fare il parsing della query string
parse_query_string(QueryString) ->
  % Split della query string in base al carattere '&'
  Pairs = string:tokens(QueryString, "&"),

  % Parsing delle coppie chiave-valore (game_id=...&player=...)
  lists:foldl(fun(Pair, Acc) ->
    case string:split(Pair, "=") of
      [Key, Value] when Key == "game_id" -> {Value, element(2, Acc)};
      [Key, Value] when Key == "player" -> {element(1, Acc), Value};
      _ -> Acc
    end
              end, {undefined, undefined}, Pairs).

%%------------------------------------------------------------------------------
%% @author SaveMos
%% @copyright (C) 2025, <FleetFra>
%% @doc Handles incoming WebSocket messages.
%% @param Msg The received WebSocket message (expected to be in JSON format).
%% @param State The current state of the WebSocket connection.
%% @return A tuple containing the response message and updated state.
%%------------------------------------------------------------------------------
websocket_handle({text, Msg}, State) ->
  Response = fleetfra_chin_handler:process_request(Msg), %% Process the request using the game logic handler.
  {reply, {text, Response}, State};   %% Send the response back to the WebSocket client.

%%------------------------------------------------------------------------------
%% @author SaveMos
%% @copyright (C) 2025, <FleetFra>
%% @doc Handles unexpected or unsupported WebSocket data.
%% @param _Data The received data that is not handled explicitly.
%% @param State The current state.
%% @return A tuple indicating that the connection remains open.
%%------------------------------------------------------------------------------
websocket_handle(_Data, State) ->
  {ok, State}.

%%------------------------------------------------------------------------------
%% @author SaveMos
%% @copyright (C) 2025, <FleetFra>
%% @doc Handles WebSocket messages sent from Erlang processes.
%% This can be used to push messages from the server to the client.
%% @param Info The message received from another process.
%% @param State The current state.
%% @return A tuple indicating that the connection remains open.
%%------------------------------------------------------------------------------
websocket_info({game_update, Response}, Req, State) ->
  io:format("Player2 received game update: ~p~n", [Response]),
  Message = jsx:encode(Response),
  {reply, {text, Message}, Req, State};
websocket_info(_Info, Req, State) ->
  {ok, Req, State}.

websocket_info(_Info, State) ->
  io:format("websocket_info received: ~p~n", [_Info]),
  {ok, State}.





%%------------------------------------------------------------------------------
%% @author SaveMos
%% @copyright (C) 2025, <FleetFra>
%% @doc Called when the WebSocket connection is closed.
%% @param Reason The reason for termination.
%% @param _Req The WebSocket request.
%% @param _State The final state before termination.
%% @return `ok` to indicate successful cleanup.
%%------------------------------------------------------------------------------
terminate(_Reason, _Req, _State) ->
  ok.
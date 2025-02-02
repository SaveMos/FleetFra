-module(fleetfra_chin_ws_handler).
-behaviour(cowboy_handler).
-author("SaveMos").
-export([init/2, websocket_handle/2, websocket_info/2, terminate/2]).

%%-------------------------------------------------------------------
%% @author SaveMos
%% @copyright (C) 2025, <FleetFra>
%% @doc
%% This handler manages WebSocket connections. It forwards the WebSocket
%% message to the existing `fleetfra_chin_handler` for processing.
%%-------------------------------------------------------------------
init(Req, State) ->
  %% WebSocket handshake is handled by Cowboy automatically.
  io:format("WebSocket message received: ~p~n", [Req]),
  websocket_handle(Req , State),
  {ok, Req, State}.

%%-------------------------------------------------------------------
%% @author SaveMos
%% @copyright (C) 2025, <FleetFra>
%% @doc
%% Handles WebSocket messages. Decodes the message, then forwards it to
%% the existing `fleetfra_chin_handler` for processing the game-related logic.
%% @param Msg The WebSocket message in text format.
%% @param State The state of the WebSocket connection.
%% @return {ok, Response, State} The response to send back to the WebSocket client.
%%-------------------------------------------------------------------
websocket_handle({text, Msg}, State) ->
  %io:format("SYSTEM: WebSocket message received!~n ~p ~n", [Msg]),

  %% Decode the incoming WebSocket message (assumed to be JSON).
  case fleetfra_chin_handler:parse_json(Msg) of
    {ok, ParsedJson} ->
      %io:format("Parsed JSON: ~p~n", [ParsedJson]),
      %% Forward the parsed message to `fleetfra_chin_handler` for processing
      Response = fleetfra_chin_handler:process_request(ParsedJson),
      %io:format("Response to send: ~s~n", [Response]),
      {ok, {text, Response}, State};

    {error, Reason} ->
      %io:format("Failed to parse message. Reason: ~s~n", [Reason]),
      Response = "{\"error\":\"Invalid message format\"}",
      {ok, {text, Response}, State}
  end;

websocket_handle({binary, _BinMsg}, State) ->
  io:format("Received binary message~n"),
  {ok, State};

websocket_handle(_Data, State) ->
  %% Handle any other types of data (e.g., binary).
  io:format("Received unsupported data type: ~p~n", [_Data]),
  {ok, State}.

%%-------------------------------------------------------------------
%% @author SaveMos
%% @copyright (C) 2025, <FleetFra>
%% @doc
%% Handles additional WebSocket data (e.g., ping/pong or control frames).
%% @param Info WebSocket control frames or other info.
%% @param State The state of the WebSocket connection.
%% @return {ok, State}.
%%-------------------------------------------------------------------
websocket_info(_Info, State) ->
  io:format("WebSocket info received: ~p~n", [_Info]),
  {ok, State}.

%%-------------------------------------------------------------------
%% @author SaveMos
%% @copyright (C) 2025, <FleetFra>
%% @doc
%% Terminates the WebSocket connection if needed.
%% @param _Reason The reason for termination.
%% @param _State The state of the WebSocket connection.
%% @return ok.
%%-------------------------------------------------------------------
terminate(_Reason, _State) ->
  io:format("WebSocket connection terminated~n"),
  ok.

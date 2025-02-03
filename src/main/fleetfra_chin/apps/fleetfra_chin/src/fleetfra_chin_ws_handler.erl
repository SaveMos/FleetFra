-module(fleetfra_chin_ws_handler).
-behaviour(cowboy_websocket).
-author("SaveMos").
-export([init/2, websocket_init/1, websocket_handle/2, websocket_info/2, terminate/3]).

%%-------------------------------------------------------------------
%% @author SaveMos
%% @copyright (C) 2025, <FleetFra>
%% @doc
%% WebSocket handler for FleetFra Chin.
%% This module manages WebSocket connections, processing incoming messages
%% and delegating request handling to `fleetfra_chin_handler`.
%%-------------------------------------------------------------------

%% @doc Initializes the WebSocket connection.
%% @param Req The incoming request.
%% @param State The initial state of the connection.
%% @return A tuple indicating that this is a WebSocket connection.
init(Req, State) ->
  {cowboy_websocket, Req, State}.

%% @doc Called when the WebSocket connection is initialized.
%% @param State The state of the WebSocket connection.
%% @return A tuple indicating success and returning the state.
websocket_init(State) ->
  {ok, State}.

%% @doc Handles incoming WebSocket messages.
%% @param Msg The received WebSocket message (expected to be in JSON format).
%% @param State The current state of the WebSocket connection.
%% @return A tuple containing the response message and updated state.
websocket_handle({text, Msg}, State) ->
  %% Decode the incoming JSON message
  DecodedMessage = fleetfra_chin_handler:parse_json(Msg),

  %% Process the request using the game logic handler
  Response = fleetfra_chin_handler:process_request(DecodedMessage),

  %% Send the response back to the WebSocket client
  {reply, {text, Response}, State};

%% @doc Handles unexpected or unsupported WebSocket data.
%% @param _Data The received data that is not handled explicitly.
%% @param State The current state.
%% @return A tuple indicating that the connection remains open.
websocket_handle(_Data, State) ->
  {ok, State}.

%% @doc Handles WebSocket messages sent from Erlang processes.
%% This can be used to push messages from the server to the client.
%% @param Info The message received from another process.
%% @param State The current state.
%% @return A tuple indicating that the connection remains open.
websocket_info(_Info, State) ->
  {ok, State}.

%% @doc Called when the WebSocket connection is closed.
%% @param Reason The reason for termination.
%% @param _Req The WebSocket request.
%% @param _State The final state before termination.
%% @return `ok` to indicate successful cleanup.
terminate(_Reason, _Req, _State) ->
  ok.
-module(alternative_handler).
-export([init/2, websocket_init/1, websocket_handle/2, websocket_info/2, terminate/3]).
-behaviour(cowboy_websocket).

init(Req, State) ->
  %% Estrai solo l'header "sec-websocket-key" e il metodo della richiesta
  Method = cowboy_req:method(Req),
  Path = cowboy_req:path(Req),
  Headers = cowboy_req:headers(Req),
  WebSocketKey = maps:get(<<"sec-websocket-key">>, Headers, <<"N/A">>),

  %% Stampa SOLO il payload essenziale
  io:format("WebSocket Request - Method: ~p, Path: ~p, Sec-WebSocket-Key: ~p~n",
    [Method, Path, WebSocketKey]),

  {cowboy_websocket, Req, State}.

websocket_init(State) ->
  io:format("WebSocket connection initialized~n"),
  {ok, State}.

websocket_handle({text, Msg}, State) ->
  %% Stampa solo il messaggio ricevuto (payload)
  case fleetfra_chin_handler:parse_json(Msg) of
    {ok, ParsedJson} ->
      %io:format("Parsed JSON: ~p~n", [ParsedJson]),
      %% Forward the parsed message to `fleetfra_chin_handler` for processing
      Response = fleetfra_chin_handler:process_request(ParsedJson);
      %io:format("Response to send: ~s~n", [Response]),

    {error, Reason} ->
      %io:format("Failed to parse message. Reason: ~s~n", [Reason]),
      Response = "{\"error\":\"Invalid message format\"}"
  end,
  Req2 = cowboy_req:reply(200, #{<<"content-type">> => <<"application/json">>}, Response),
  {ok, Req2, State};

websocket_handle(_Data, State) ->
  {ok, State}.

websocket_info(Info, State) ->
  {ok, State}.

terminate(Reason, _Req, _State) ->
  ok.

%%%-------------------------------------------------------------------
%%% @author Saverio
%%% @copyright (C) 2025, <FleetFra>
%%% @doc
%%%
%%% @end
%%% Created : 28. gen 2025 11:25
%%%-------------------------------------------------------------------
-author("Saverio").
-module(fleetfra_web_server).
-export([start/0, handle_request/1, send_message/2, process_json/1, process_game_move/1]).

%% Start the web server
start() ->
  inets:start(),
  Port = config:get_port(),  %% Retrieve the port from config
  httpd:start(Port, {module, fleetfra_web_server}).

%% Handle the incoming request
handle_request(Req) ->
  case httpd_util:parse_query(Req) of
    {ok, Json} ->
      %% Process the JSON request
      process_json(Json);
    {error, _} ->
      {error, invalid_request}
  end.

%% Send a message to the Java server
send_message(Url, Message) ->
  %% Send the JSON message to the Java server via HTTP POST
  httpc:request(post, {Url, [], "application/json", jsx:encode(Message)}, [], []).

%% Process the received JSON request
process_json(Json) ->
  %% Extract the type of the request from the JSON safely
  case maps:find(<<"type_request">>, Json) of
    {ok, TypeRequest} ->
      %% Handle the request based on its type
      case TypeRequest of
        <<"start_game">> ->
          %% Start a new game
          case fleetfra_game_registry:start_new_game(Json) of
            {ok, GameId} ->
              %% Build and send the response JSON to the Java server
              Message = #{<<"game_id">> => GameId, <<"status">> => <<"game_started">>},
              send_message(config:java_server_url() ++ "/start_game", Message);
            {error, already_exists} ->
              Message = #{<<"game_id">> => <<>>, <<"status">> => <<"game_already_exists">>},
              send_message(config:java_server_url() ++ "/start_game", Message)
          end;

        <<"make_move">> ->
          %% Process the player's move
          case process_game_move(Json) of
            {ok, Message} ->
              send_message(config:java_server_url() ++ "/move_made", Message);
            {error, Message} ->
              send_message(config:java_server_url() ++ "/move_made", Message)
          end;

        _ ->
          %% Handle unknown request type
          Message = #{<<"status">> => <<"unknown_request_type">>},
          send_message(config:java_server_url() ++ "/error", Message)
      end;

    _ ->
      %% Handle the case where "type_request" is missing or invalid
      Message = #{<<"status">> => <<"missing_type_request">>},
      send_message(config:java_server_url() ++ "/error", Message)
  end.

%% Receiving the game move request
process_game_move(Json) ->
  %% Extract the move data and game ID from the JSON safely
  case maps:find(<<"move_data">>, Json) of
    {ok, MoveData} ->
      case maps:find(<<"game_id">>, Json) of
        {ok, GameId} ->
          %% Retrieve the game process by GameId
          case fleetfra_game_registry:get_game_process(GameId) of
            {ok, GameProcess} ->
              %% Send the move data to the game process
              GameProcess ! {move, MoveData},
              %% Send a response back to the Java server
              Message = #{<<"game_id">> => GameId, <<"status">> => <<"move_processed">>},
              {ok, Message};

            {error, not_found} ->
              %% Send a response indicating that the game was not found
              Message = #{<<"game_id">> => GameId, <<"status">> => <<"game_not_found">>},
              {error, Message}
          end;

        _ ->
          %% Missing or invalid game_id in the JSON
          Message = #{<<"status">> => <<"invalid_game_id">>},
          {error, Message}
      end;

    _ ->
      %% Missing or invalid move_data in the JSON
      Message = #{<<"status">> => <<"invalid_move_data">>},
      {error, Message}
  end.


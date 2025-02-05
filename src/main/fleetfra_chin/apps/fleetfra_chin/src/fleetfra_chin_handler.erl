-module(fleetfra_chin_handler).
-behaviour(cowboy_handler).
-author("SaveMos").
-export([init/2 , process_request/1 , parse_json/1 , build_response/1]).

%%==============================================================================%%
%% @author SaveMos
%% @copyright (C) 2025, <FleetFra>
%% @doc
%% Handles incoming HTTP requests for starting a game or making a move.
%% Parses the request body, extracts the game information, and calls the appropriate game logic functions.
%% Created : 01. feb 2025 09:53
%% @end
%% @param Req The HTTP request object.
%% @param State The state of the Cowboy handler.
%% @return {ok, Req2, State} The updated request and state.
%%==============================================================================%%
init(Req, State) ->
    {ok, Body, Req1} = cowboy_req:read_body(Req),
    Response = process_request(Body),
    Req2 = cowboy_req:reply(200, #{<<"content-type">> => <<"application/json">>}, Response, Req1),
    {ok, Req2, State}.

%%-------------------------------------------------------------------
%% @author SaveMos
%% @copyright (C) 2025, <FleetFra>
%% @doc
%% Processes the request, which is a JSON object representing a game action.
%% @end
%% @param ParsedJson The parsed JSON request.
%% @return A JSON-encoded response.
%%-------------------------------------------------------------------
process_request(Body) ->
    ParsedJson = parse_json({jsx , Body}),
    GameID = maps:get(<<"game_id">>, ParsedJson),
    TypeRequest = maps:get(<<"type_request">>, ParsedJson),
    case TypeRequest of
        <<"start_game">> ->
            Player1 = maps:get(<<"player1">>, ParsedJson),
            Player2 = maps:get(<<"player2">>, ParsedJson),
            Battlefield1 = maps:get(<<"player1_battlefield">>, ParsedJson),
            Battlefield2 = maps:get(<<"player2_battlefield">>, ParsedJson),
            fleetfra_game:start_game(GameID, {Player1, Player2, Battlefield1, Battlefield2}),
            build_response(<<"OK: Game started">>);

        <<"start_game_client">> ->
            Player = maps:get(<<"player">>, ParsedJson),
            Battlefield = maps:get(<<"player_battlefield">>, ParsedJson),
            fleetfra_game:start_game_client(GameID, {Player, Battlefield}),
            build_response(<<"OK: Game started">>);

        <<"change_turn">> ->
            case fleetfra_game:change_turn(GameID) of
                {ok, proceed} -> build_response(<<"OK: Turn changed">>);
                {error, game_not_found} -> build_response(<<"ERROR: Game not found">>)
            end;

        <<"get_game_info">> ->
            case fleetfra_game:get_game_info(GameID) of
                {ok, JsonResponse} -> JsonResponse;
                {error, game_not_found} -> build_response(<<"ERROR: Game not found">>)
            end;

        <<"make_move">> ->
            Player = maps:get(<<"player">>, ParsedJson),
            Move = maps:get(<<"move">>, ParsedJson),
            Row = maps:get(<<"row">>, Move),
            Col = maps:get(<<"col">>, Move),
            case fleetfra_game:make_move(GameID, {Player, {Row, Col}}) of
                {ok, NewValue} -> build_response(<<"OK: Move accepted [", (integer_to_binary(NewValue))/binary, "]">>);
                {error, invalid_move} -> build_response(<<"Invalid move">>);
                {error, out_of_bound_coordinates} -> build_response(<<"INVALID MOVE: Out of bound coordinates">>);
                {error, not_integer} -> build_response(<<"INVALID MOVE: Coordinates must be integers">>);
                {error, not_your_turn} -> build_response(<<"TURN ERROR: Not your turn">>);
                {error, player_not_found} -> build_response(<<"ERROR: Player not found">>);
                {error, game_not_found} -> build_response(<<"ERROR: Game not found">>);
                {error , game_not_initiated} -> build_response(<<"ERROR: Game found but not initiated">>);
                {fin, winner} -> build_response(<<"VICTORY">>);
                {fin, loser} -> build_response(<<"DEFEAT">>)
            end;
        _ -> build_response(<<"Unknown request type">>)
    end.

%%-------------------------------------------------------------------
%% @author SaveMos
%% @copyright (C) 2025, <FleetFra>
%% @doc
%% Parses the incoming JSON request body and returns a map.
%% @end
%% @param Body The JSON body of the request.
%% @return A map representing the parsed JSON.
%%-------------------------------------------------------------------
parse_json({jsx , Body}) ->
    %% Assume JSON parsing succeeds using JSX.
    jsx:decode(Body, [{return_maps, true}]);
parse_json({jiffy , Body}) ->
    %% Assume JSON parsing succeeds using JSX.
    jiffy:decode(Body).

%%-------------------------------------------------------------------
%% @author SaveMos
%% @copyright (C) 2025, <FleetFra>
%% @doc
%% Builds a JSON response string to send back to the client.
%% @end
%% @param Message The message to include in the response.
%% @return The JSON-encoded response.
%%-------------------------------------------------------------------

build_response(Message) ->
    decode_json(Message).

%%-------------------------------------------------------------------
%% @author SaveMos
%% @copyright (C) 2025, <FleetFra>
%% @doc
%% Decode a MAP into a JSON message.
%% @end
%%-------------------------------------------------------------------

decode_json(Message) ->
    %% Encode response using JSX
    jsx:encode(#{<<"message">> => Message}).


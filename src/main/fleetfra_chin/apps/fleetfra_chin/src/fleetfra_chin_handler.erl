-module(fleetfra_chin_handler).
-behaviour(cowboy_handler).
-author("SaveMos").
-export([init/2]).

%%-------------------------------------------------------------------
%% @author SaveMos
%% @copyright (C) 2025, <FleetFra>
%% @doc
%% Handles incoming HTTP requests for starting a game or making a move.
%% Parses the request body, extracts the game information, and calls the appropriate game logic functions.
%% @param Req The HTTP request object.
%% @param State The state of the Cowboy handler.
%% @return {ok, Req2, State} The updated request and state.
%%-------------------------------------------------------------------
init(Req, State) ->
    %io:format("REPORT: request received!~n"),
    %% Parse JSON body
    {ok, Body, Req1} = cowboy_req:read_body(Req),

    %% Decode JSON and extract the game ID and request type
    ParsedJson = parse_json(Body),

    GameID = maps:get(<<"game_id">>, ParsedJson),
    TypeRequest = maps:get(<<"type_request">>, ParsedJson),

    %% Handle the request based on its type
    case TypeRequest of
        <<"start_game">> ->
            %io:format("start-game request received!~n"),
            Player1 = maps:get(<<"player1">>, ParsedJson),
            Player2 = maps:get(<<"player2">>, ParsedJson),
            Battlefield1 = maps:get(<<"player1_battlefield">>, ParsedJson),
            Battlefield2 = maps:get(<<"player2_battlefield">>, ParsedJson),
            %% Start the game with the provided battlefields
            fleetfra_game:start_game(GameID, {Player1, Player2, Battlefield1, Battlefield2}),
            %io:format("Battlefield1: ~p~n", [Battlefield1]),
            %io:format("Battlefield2: ~p~n", [Battlefield2]),
            Response = build_response(<<"OK: Game started">>);

        <<"make_move">> ->
            %io:format("move request received!~n"),
            Player =  maps:get(<<"player">>, ParsedJson),
            Move = maps:get(<<"move">>, ParsedJson),
            Row = maps:get(<<"row">>, Move),
            Col = maps:get(<<"col">>, Move),
            %% Make the move
            case fleetfra_game:make_move(GameID, {Player, {Row, Col}}) of
                {ok, _} -> Response = build_response(<<"OK: Move accepted">>);
                {error, invalid_move} -> Response = build_response(<<"Invalid move">>);
                {error, out_of_bound_coordinates} -> Response = build_response(<<"INVALID MOVE: Out of bound coordinates">>);
                {error, not_integer} -> Response = build_response(<<"INVALID MOVE: Coordinates must be integers">>);
                {error, not_your_turn} -> Response = build_response(<<"TURN ERROR: Not your turn">>);
                {error, player_not_found} -> Response = build_response(<<"ERROR: Player not found">>);
                {error, game_not_found} -> Response = build_response(<<"ERROR: Game not found">>);
                {fin, winner} -> Response = build_response(<<"VICTORY">>);
                {fin, loser} -> Response = build_response(<<"DEFEAT">>)
            % {"message":"Move accepted"}
            % {"message":"VICTORY"}
            end;

        _ ->
            Response = build_response(<<"Unknown request type">>)
    end,

    Req2 = cowboy_req:reply(200, #{<<"content-type">> => <<"application/json">>}, Response, Req1),
    {ok, Req2, State}.

%%-------------------------------------------------------------------
%% @author SaveMos
%% @copyright (C) 2025, <FleetFra>
%% @doc
%% Parses the incoming JSON request body and returns a map.
%% @param Body The JSON body of the request.
%% @return A map representing the parsed JSON.
%%-------------------------------------------------------------------
parse_json(Body) ->
    %% Assume JSON parsing succeeds
    jsx:decode(Body, [return_maps]).

%%-------------------------------------------------------------------
%% @author SaveMos
%% @copyright (C) 2025, <FleetFra>
%% @doc
%% Builds a JSON response string to send back to the client.
%% @param Message The message to include in the response.
%% @return The JSON-encoded response.
%%-------------------------------------------------------------------
build_response(Message) ->
    jsx:encode(#{<<"message">> => Message}).

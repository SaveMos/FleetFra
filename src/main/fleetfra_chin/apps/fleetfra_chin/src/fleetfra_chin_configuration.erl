%%%-------------------------------------------------------------------
%% @author SaveMos
%% @copyright (C) 2025, <FleetFra>
%%% @doc
%%% Constants for the web server
%%% @end
%%% Created : 29. gen 2025 09:59
%%%-------------------------------------------------------------------
-module(fleetfra_chin_configuration).
-author("SaveMos").

%% API
-export([
  get_port/0 ,
  get_auto_clean_period/0 ,
  get_max_match_age/0 ,
  get_battlefield_dimension/0,
  get_game_info_request_schema_path/0,
  get_start_game_client_request_schema_path/0 ,
  get_move_request_schema_path/0,
  get_start_game_client_request_schema/0
]). % Export all functions.

%% get_port/0: Returns the port number for the web server.
get_port() -> 8080.

%% get_auto_clean_period/0: Returns the period for automatic cleaning in milliseconds.
%% The value is set to 1 hour (3600 seconds * 1000 milliseconds).
get_auto_clean_period() -> 3600*1000.

%% get_max_match_age/0: Returns the maximum match age in seconds.
%% The value is set to 24 hours (24 * 3600 seconds).
get_max_match_age() -> 24*3600.

get_battlefield_dimension() -> 10.

get_game_info_request_schema_path() -> "json_schema/get_game_info_schema.json".

get_move_request_schema_path() -> "json_schema/move_message_schema.json".

get_start_game_client_request_schema_path() -> "src/json_schema/start_game_client_message_schema.json".

get_start_game_client_request_schema() ->
  [{<<"game_id">>, [{<<"type">>, <<"string">>}]},
    {<<"player">>, [{<<"type">>, <<"string">>}]},
    {<<"player_battlefield">>, [{<<"type">>, <<"array">>},
      {<<"items">>, [{<<"type">>, <<"object">>},
        {<<"properties">>, [
          {<<"col">>, [{<<"type">>, <<"integer">>}]},
          {<<"row">>, [{<<"type">>, <<"integer">>}]},
          {<<"value">>, [{<<"type">>, <<"integer">>}]}
        ]}
      ]}]},
    {<<"type_request">>, [{<<"type">>, <<"string">>}]}
  ].
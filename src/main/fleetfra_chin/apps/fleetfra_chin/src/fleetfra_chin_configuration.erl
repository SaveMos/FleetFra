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
-export([get_port/0 , get_auto_clean_period/0, get_max_match_age/0]).

get_port() -> 8080. %% Returns the port number for the web server.

get_auto_clean_period() -> 3600*1000. % 1 hour in milliseconds.

get_max_match_age() -> 24*3600.  % 24 hours in seconds.

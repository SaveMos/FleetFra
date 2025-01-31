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

%% get_port/0: Returns the port number for the web server.
get_port() -> 8080.

%% get_auto_clean_period/0: Returns the period for automatic cleaning in milliseconds.
%% The value is set to 1 hour (3600 seconds * 1000 milliseconds).
get_auto_clean_period() -> 3600*1000.

%% get_max_match_age/0: Returns the maximum match age in seconds.
%% The value is set to 24 hours (24 * 3600 seconds).
get_max_match_age() -> 24*3600.

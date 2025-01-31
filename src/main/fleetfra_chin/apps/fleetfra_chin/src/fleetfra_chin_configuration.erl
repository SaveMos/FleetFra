%%%-------------------------------------------------------------------
%%% @author Saverio
%%% @copyright (C) 2025, <COMPANY>
%%% @doc
%%% Constants for the web server
%%% @end
%%% Created : 29. gen 2025 09:59
%%%-------------------------------------------------------------------
-module(fleetfra_chin_configuration).
-author("Saverio").

%% API
-export([get_port/0]).

get_port() -> 8080. %% Returns the port number for the web server

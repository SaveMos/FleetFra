%%==============================================================================%%
%% @author SaveMos
%% @copyright (C) 2025, <FleetFra>
%% @doc
%% Constants for the web server.
%% @end
%% Created : 29. Jan 2025 09:59
%%==============================================================================%%

-module(fleetfra_chin_configuration).
-author("SaveMos").

%% API
-export([
  get_port/0 ,
  get_auto_clean_period/0 ,
  get_max_match_age/0 ,
  get_battlefield_dimension/0
]). % Export all functions.

%%%===================================================================
%%% API functions
%%%===================================================================

%%-------------------------------------------------------------------
%% @author SaveMos
%% @copyright (C) 2025, <FleetFra>
%% @doc
%% Returns the port number for the web server.
%% @returns 8080, the port number for the server.
%% @end
%%-------------------------------------------------------------------
get_port() -> 8080.

%%-------------------------------------------------------------------
%% @author SaveMos
%% @copyright (C) 2025, <FleetFra>
%% @doc
%% Returns the period for automatic cleaning in milliseconds.
%% The value is set to 1 hour (3600 seconds * 1000 milliseconds).
%% @returns 3600000, the auto-clean period in milliseconds.
%% @end
%%-------------------------------------------------------------------
get_auto_clean_period() -> 3600*1000.

%%-------------------------------------------------------------------
%% @author SaveMos
%% @copyright (C) 2025, <FleetFra>
%% @doc
%% Returns the maximum match age in seconds.
%% The value is set to 24 hours (24 * 3600 seconds).
%% @returns 86400, the maximum match age in seconds.
%% @end
%%-------------------------------------------------------------------
get_max_match_age() -> 24*3600.

%%-------------------------------------------------------------------
%% @author SaveMos
%% @copyright (C) 2025, <FleetFra>
%% @doc
%% Returns the battlefield dimensions for the game.
%% @returns 10, the size of the battlefield dimension.
%% @end
%%-------------------------------------------------------------------
get_battlefield_dimension() -> 10.

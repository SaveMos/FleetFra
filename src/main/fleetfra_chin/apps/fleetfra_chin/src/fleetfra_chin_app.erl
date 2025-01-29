%%%-------------------------------------------------------------------
%% @doc fleetfra_chin public API
%% @end
%%%-------------------------------------------------------------------

-module(fleetfra_chin_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    Dispatch = cowboy_router:compile([
        { <<"10.2.1.30">>, [{<<"/">>, fleetfra_chin_handler, []}] }
    ]),
    {ok, _} = cowboy:start_clear(
        hello_listener,
        [{port, 8080}],
        #{env => #{dispatch => Dispatch}}
    ),
    fleetfra_chin_sup:start_link().

stop(_State) ->
    ok.

%% internal functions

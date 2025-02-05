%%%-------------------------------------------------------------------
%% @author SaveMos
%% @copyright (C) 2025, <FleetFra>
%% @doc
%% This module starts the HTTP server using Cowboy.
%%
%% The module follows the standard Erlang/OTP application behavior,
%% implementing the `start/2` and `stop/1` functions.
%%
%% @see cowboy
%% @see inet:getifaddrs/0
%% @see cowboy_router:compile/1
%% @end
%%%-------------------------------------------------------------------

-module(fleetfra_chin_app).
-author("SaveMos").
-behaviour(application).
%% API
-export([start/2, stop/1]).

%%%-------------------------------------------------------------------
%% @author SaveMos
%% @copyright (C) 2025, <FleetFra>
%% @doc
%% Starts the HTTP server.
%%
%% @param _StartType Ignored.
%% @param _StartArgs Ignored.
%% @return {ok, Pid} where Pid is the supervisor process ID.
%%%-------------------------------------------------------------------
start(_StartType, _StartArgs) ->
  Dispatch = cowboy_router:compile([
    % Gestisce le richieste HTTP sulla rotta "/erl"
    %{ '_' , [{"/erl", fleetfra_chin_handler, []}] },
    % Gestisce le richieste WebSocket sulla rotta "/ws"
    { '_', [{"/ws", fleetfra_chin_ws_handler, []}]}
    ]),

  % Avvia il server HTTP Cowboy
  % It's a mapping of the connection from some remote host's path to cowboy_handler.
  % To do that, we use cowboy_router:compile/1 which takes a parameter of type cowboy_router:routes().
  cowboy:start_clear(
    hello_listener,
    [{port, fleetfra_chin_configuration:get_port()}], % the service port.
    #{env => #{dispatch => Dispatch}}
  ),

  % Avvia il game_state_manager e il supervisore
  game_state_manager:start_link(), %% Starts the ETS manager.
  fleetfra_chin_sup:start_link().

%%%-------------------------------------------------------------------
%% @author SaveMos
%% @copyright (C) 2025, <FleetFra>
%% @doc
%% Stops the application.
%%
%% This function is required by the OTP application behavior but does nothing.
%%
%% @param _State Ignored.
%% @return ok.
%%%-------------------------------------------------------------------
stop(_State) ->
  ok.

%% internal functions
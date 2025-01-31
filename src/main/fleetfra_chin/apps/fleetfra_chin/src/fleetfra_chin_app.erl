%%%-------------------------------------------------------------------
%% @doc
%% This module starts the HTTP server using Cowboy.
%%
%% It dynamically retrieves the IP address of the "eth0" network interface
%% and uses it to configure Cowboy's routing. If "eth0" is not found,
%% it falls back to using "127.0.0.1".
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

-behaviour(application).

-export([start/2, stop/1]).

%%%-------------------------------------------------------------------
%% @doc
%% Starts the HTTP server.
%%
%% Retrieves the IP address of "eth0" and sets up a Cowboy listener
%% on that address. If "eth0" is unavailable, "127.0.0.1" is used instead.
%%
%% @param _StartType Ignored.
%% @param _StartArgs Ignored.
%% @return {ok, Pid} where Pid is the supervisor process ID.
%%%-------------------------------------------------------------------
start(_StartType, _StartArgs) ->
  % Get the IP address of eth0
  {ok, IfAddrs} = inet:getifaddrs(),
  IP = case lists:keyfind("eth0", 1, IfAddrs) of
         {_, AddrList} ->
           case lists:keyfind(addr, 1, AddrList) of
             {addr, IPAddr} -> inet:ntoa(IPAddr);
             false -> "127.0.0.1"
           end;
         false -> "127.0.0.1"
       end,

  % We first need to set what cowboy calls "route".
  Dispatch = cowboy_router:compile([
    { list_to_binary(IP), [{<<"/">>, fleetfra_chin_handler, []}] } % Automatically use the eth0 IP
  ]),

  % It's a mapping of the connection from some remote host's path to cowboy_handler.
  % To do that, we use cowboy_router:compile/1 which takes a parameter of type cowboy_router:routes().
  {ok, _} = cowboy:start_clear(
    hello_listener,
    [{port, fleetfra_chin_configuration:get_port()}],
    #{env => #{dispatch => Dispatch}}
  ),
  game_state_manager:start_link(), %% Starts the ETS manager.
  fleetfra_chin_sup:start_link().

%%%-------------------------------------------------------------------
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
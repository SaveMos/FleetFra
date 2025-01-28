%%%-------------------------------------------------------------------
%%% @author Saverio
%%% @copyright (C) 2025, <FleetFra>
%%% @doc
%%%
%%% @end
%%% Created : 28. gen 2025 11:27
%%%-------------------------------------------------------------------
-module(fleetfra_app).
-author("Saverio").

-behaviour(application).
%% API
-export([start/2, stop/1]).

start(_, _) ->
  %% Starts the web server.
  fleetfra_web_server:start(),
  {ok, self()}.

stop(_) ->
  %% Stop the web server.
  ok.
